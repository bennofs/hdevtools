{-# LANGUAGE CPP                 #-}
{-# LANGUAGE MonadComprehensions #-}
module Cabal where

import           Config                                        (cProjectVersion)
import           Control.Applicative
import           Control.Monad                                 (guard)
import           Data.List
import           Data.Maybe                                    (listToMaybe)
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
    (Left _) -> error "This is a bug in parseCabalConfig" -- We said all packages are available, so this should definitely not happen
    (Right r) -> return $ fst r
  where v = return . finalizePackageDescription [] (const True) buildPlatform (CompilerId GHC ghcVersion) [] =<< parsePackageDescription contents

findBuildInfoFile :: PackageDescription -> String -> Maybe BuildInfo
findBuildInfoFile d f = listToMaybe $ filter (any (`isPrefixOf` f) . hsSourceDirs) $ allBuildInfo d

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
  [ pureIfM (doesFileExist localDB) $ "-package-db " ++ localDB
  , ifM (doesFileExist macros) ["-optP-include", "-optP" ++ macros]
  ]
  where localDB = "dist/package.conf.inplace"
        macros = "dist/build/autogen/cabal_macros.h"

getBuildInfoOptions :: BuildInfo -> Maybe [String]
getBuildInfoOptions bi = do
  o <- listToMaybe $ map snd $ filter ((== GHC) . fst) $ options bi
  let o' = map ("-i" ++) $ hsSourceDirs bi
  let o'' = map (("-X" ++) . CT.display) $ defaultExtensions bi
  let deps = targetBuildDepends bi
      o''' = map (("-package " ++) . CT.display . pkgName) deps
  return $ o' ++ o'' ++ o ++ o''' ++ ["-hide-all-packages"]

  where pkgName (Dependency n _) = n

