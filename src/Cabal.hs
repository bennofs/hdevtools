{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MonadComprehensions #-}
module Cabal where

import           Config                                        (cProjectVersion)
import           Control.Applicative
import           Control.Monad                                 (guard)
import           Data.Char
import           Data.List
import           Data.Maybe                                    (listToMaybe)
import           Data.Monoid
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
import           Text.ParserCombinators.ReadP                  (readP_to_S)

ghcVersion :: Version
ghcVersion = fst $ head $ filter (null . snd) $ readP_to_S parseVersion cProjectVersion

parseCabalConfig :: String -> ParseResult PackageDescription
parseCabalConfig contents = v >>= \v' -> case v' of
    (Left _) -> error "This is a bug in parseCabalConfig" -- We said all packages are available (const True), so this should definitely not happen
    (Right r) -> return $ fst r
  where v = return . finalizePackageDescription [] (const True) buildPlatform (CompilerId GHC ghcVersion) [] =<< parsePackageDescription contents

findBuildInfoFile :: PackageDescription -> String -> Either String BuildInfo
findBuildInfoFile d f = case buildInfos of
  [bi] -> Right bi
  []   -> exitError "No matching build target (library, executable or benchmark) found"
  _   -> exitError  "Multiple matching build configurations found"
  where buildInfos = filter (any (`isPrefixOf` f) . hsSourceDirs) $ allBuildInfo d
        sourceDirs = concatMap hsSourceDirs $ allBuildInfo d
        exitError msg = Left $ msg ++ " [Checked source directories: " ++ show sourceDirs ++ "]"

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
          return $ [packageDBFlag ++ " " ++ db | l <- lines s, "package-db:" `isPrefixOf` l
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
