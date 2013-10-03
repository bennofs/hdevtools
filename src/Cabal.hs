{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MonadComprehensions #-}
module Cabal where

import           Config                                        (cProjectVersion)
import           Control.Applicative
import           Control.Lens
import           Control.Monad                                 (guard)
import           Data.Char
import           Data.List
import           Data.Maybe                                    (listToMaybe)
import           Data.Monoid
import           Data.Ord
import           Data.Version
import           Distribution.Compiler
import qualified Distribution.ModuleName                       as MN
import           Distribution.Package                          (Dependency (..))
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (finalizePackageDescription)
import           Distribution.PackageDescription.Parse         (ParseResult, parsePackageDescription)
import           Distribution.System                           (buildPlatform)
import qualified Distribution.Text                             as CT
import           System.Directory
import           System.FilePath
import           Text.ParserCombinators.ReadP                  (readP_to_S)

ghcVersion :: Version
ghcVersion = fst $ head $ filter (null . snd) $ readP_to_S parseVersion cProjectVersion

parseCabalConfig :: String -> ParseResult PackageDescription
parseCabalConfig contents = v >>= \v' -> case v' of
    (Left _) -> error "This is a bug in parseCabalConfig" -- We said all packages are available (const True), so this should definitely not happen
    (Right r) -> return $ fst r
  where v = return . finalizePackageDescription [] (const True) buildPlatform (CompilerId GHC ghcVersion) [] =<< parsePackageDescription contents

-- | This function tests whether a given path is a prefix of another path. It returns 'Nothing' if the path isn't a prefix, otherwise
-- it returns 'Just' the number of directory components of the prefix. Note that while this function doesn't canonicalize paths, it recognizes
-- @"."@ and ignores it, but doesn't handle @".."@. It's meant to be used in infix form, like the 'isPrefixOf' function from 'Data.List'.
-- Examples:
--
-- >>> "." `prefixPathOf` "abc/ad/def.txt"
-- Just 0
--
-- >>> "../x" `prefixPathOf` "abc/ad/def"
-- Nothing -- Always Nothing, even if the current directory is @x@. This function doesn't canonicalize paths!
--
-- >>> "tests" `prefixPathOf` "tests/1213.hs"
-- Just 1 -- @tests@ has one directory component
--
-- 
prefixPathOf :: FilePath -> FilePath -> Maybe Int
"." `prefixPathOf` _ = Just 0
dir `prefixPathOf` subdir = go dirParts subdirParts
  where dirParts = filter (/= ".") $ map (under reversed $ strip '\\' . strip '/') $ splitPath dir
        subdirParts = filter (/= ".") $ map (under reversed $ strip '\\' . strip '/') $ splitPath subdir

        strip c (a:x) = if c == a then x else a:x
        strip _ _ = []
        
        go [] _ = Just 0
        go _ [] = Nothing
        go (a:as) (b:bs) 
          | a == b = succ <$> go as bs
          | otherwise = Nothing

-- | Find all maxima in a given list.
maximaBy :: (a -> a -> Ordering) -> [a] -> [a]
maximaBy _ [] = []
maximaBy cmp (a:as) = go a as
  where go x (b:bs) = case bs' of
          (m:ms) -> case x `cmp` m of
            LT -> m:ms
            EQ -> x:m:ms
            GT -> [x]
          _ -> [x]
          where bs' = go b bs
        go x [] = [x]

-- | Get ALL buildinfo, including disabled test-suites, benchmarks, etc ...
-- We still exclude things that aren't buildable.
reallyAllBuildInfo :: PackageDescription -> [BuildInfo]
reallyAllBuildInfo pkg_descr = [ bi | Just lib <- [library pkg_descr]
                                    , let bi = libBuildInfo lib
                                    , buildable bi ]
                            ++ [ bi | exe <- executables pkg_descr
                                    , let bi = buildInfo exe
                                    , buildable bi ]
                            ++ [ bi | tst <- testSuites pkg_descr
                                    , let bi = testBuildInfo tst
                                    , buildable bi]
                            ++ [ bi | tst <- benchmarks pkg_descr
                                    , let bi = benchmarkBuildInfo tst
                                    , buildable bi]

findBuildInfoFile :: PackageDescription -> String -> Either String BuildInfo
findBuildInfoFile d f = case buildInfos of
  [bi] -> if has (traverse . to (`prefixPathOf` f) . _Just) $ hsSourceDirs bi
           then Right bi
           else noMatching
  []   -> noMatching
  _   -> exitError  "Multiple matching build configurations found"
  where buildInfos = maximaBy (comparing $ maximumOf $ to hsSourceDirs . traverse . to (`prefixPathOf` f) . traverse) $ reallyAllBuildInfo d
        sourceDirs = concatMap hsSourceDirs $ allBuildInfo d
        exitError msg = Left $ msg ++ " [Checked source directories: " ++ show sourceDirs ++ "]"
        noMatching = exitError "No matching build target (library, executable or benchmark) found"

findBuildInfoModule :: PackageDescription -> String -> Maybe BuildInfo
findBuildInfoModule d m = findLibrary <|> findExe <|> findTestSuite
  where findLibrary = library d >>= \x -> libBuildInfo x <$ guard (MN.fromString m `elem` libModules x)
        findExe = fmap buildInfo $ listToMaybe $ filter ((MN.fromString m `elem`) . exeModules) $ executables d
        findTestSuite = fmap testBuildInfo $ listToMaybe $ filter ((MN.fromString m `elem`) . testModules) $ testSuites d

if' :: a -> a -> Bool -> a
if' a _ False = a
if' _ a True = a

pureIfM :: (Functor m, Applicative f, Alternative f) => m Bool -> a -> m (f a)
pureIfM w = ifM w . pure

ifM :: (Functor m, Alternative f) => m Bool -> f a -> m (f a)
ifM w v = if' empty v <$> w

packageDBFlag :: String
#if __GLASGLOW_HASKELL__ >= 706
packageDBFlag = "-package-db"
#else
packageDBFlag = "-package-conf"
#endif

cabalMiscOptions :: IO [String]
cabalMiscOptions =
  fmap concat $ sequence
  [ pureIfM (doesFileExist localDB) $ packageDBFlag ++ " " ++ localDB
  , ifM (doesFileExist macros) ["-optP-include", "-optP" ++ macros]
  , doesFileExist sandboxConf >>= if' (return []) getSandboxDBs
  ]
  where localDB = "dist/package.conf.inplace"
        macros = "dist/build/autogen/cabal_macros.h"
        sandboxConf = "cabal.sandbox.config"
        getSandboxDBs = do
          s <- readFile "cabal.sandbox.config"
          return [packageDBFlag ++ " " ++ db | l <- lines s, "package-db:" `isPrefixOf` l
                                               , let db = dropWhile isSpace $ drop (length "package-db:") l]

getBuildInfoOptions :: BuildInfo -> [String]
getBuildInfoOptions bi = concat
  [ concat ghcOpts
  , map (mappend "-i") $ hsSourceDirs bi
  , map (mappend "-X" . CT.display) $ defaultExtensions bi
  , map (mappend "-package" . CT.display . pkgName)  $ targetBuildDepends bi
  , ["-hide-all-packages"]
  ]
  where pkgName (Dependency n _) = n
        ghcOpts = map snd $ filter ((== GHC) . fst) $ options bi
