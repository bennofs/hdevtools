{-# LANGUAGE MonadComprehensions #-}
module Cabal where

import Control.Applicative((<|>), (<$), Applicative, Alternative, pure, empty, (<$>))
import Control.Monad (guard, (>=>))
import Control.Monad.IfElse
import Config (cProjectVersion)
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (listToMaybe)
import Data.Version (Version, parseVersion)
import Distribution.Compiler (CompilerId(..), CompilerFlavor(..))
import qualified Distribution.ModuleName as MN
import Distribution.Package (Dependency(..))
import Distribution.PackageDescription (PackageDescription(..), FlagAssignment, BuildInfo, library, libModules, libBuildInfo, exeModules, buildInfo, testBuildInfo, testModules, testSuites, options, hsSourceDirs, defaultExtensions, targetBuildDepends, allBuildInfo, hsSourceDirs)
import Distribution.PackageDescription.Parse (parsePackageDescription, ParseResult)
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.System (buildPlatform)
import qualified Distribution.Text as CT
import System.Directory
import Text.ParserCombinators.ReadP (readP_to_S)

ghcVersion :: Version
ghcVersion = fst $ head $ filter (null . snd) $ readP_to_S parseVersion cProjectVersion

parseCabalConfig :: String -> ParseResult PackageDescription
parseCabalConfig contents = v >>= \v' -> case v' of
    (Left d) -> error "This is a bug in parseCabalConfig" -- We said all packages are available, so this should definitely not happen
    (Right r) -> return $ fst r
  where v = return . finalizePackageDescription [] (const True) buildPlatform (CompilerId GHC ghcVersion) [] =<< parsePackageDescription contents

findBuildInfoFile :: PackageDescription -> String -> Maybe BuildInfo
findBuildInfoFile d f = listToMaybe $ filter (any (`isPrefixOf` f) . hsSourceDirs) $ allBuildInfo d

findBuildInfoModule :: PackageDescription -> String -> Maybe BuildInfo
findBuildInfoModule d m = findLibrary <|> findExecutable <|> findTestSuite
  where findLibrary = library d >>= \x -> libBuildInfo x <$ guard (MN.fromString m `elem` libModules x)
        findExecutable = fmap buildInfo $ listToMaybe $ filter ((MN.fromString m `elem`) . exeModules) $ executables d
        findTestSuite = fmap testBuildInfo $ listToMaybe $ filter ((MN.fromString m `elem`) . testModules) $ testSuites d

if' :: a -> a -> Bool -> a
if' a _ False = a
if' _ a True = a

pureIfM :: (Functor m, Applicative f, Alternative f) => m Bool -> a -> m (f a)
pureIfM w = ifM w . pure

ifM :: (Functor m, Alternative f) => m Bool -> f a -> m (f a)
ifM w v = if' empty v <$> w

cabalMiscOptions :: IO [String]
cabalMiscOptions = 
  fmap concat $ sequence $
  [ pureIfM (doesFileExist localDB) $ "-package-db " ++ localDB
  , ifM (doesFileExist macros) $ ["-optP-include", "-optP" ++ macros]
  ]
  where localDB = "dist/package.conf.inplace"
        macros = "dist/build/autogen/cabal_macros.h"

getBuildInfoOptions :: BuildInfo -> Maybe [String]
getBuildInfoOptions bi = do
  o <- listToMaybe $ map snd $ filter ((== GHC) . fst) $ options bi
  let o' = map ("-i" ++) $ hsSourceDirs bi
  let o'' = map ("-X" ++) $ map CT.display $ defaultExtensions bi
  let deps = targetBuildDepends bi
      o''' = map (("-package " ++) . CT.display . pkgName) deps
  return $ o' ++ o'' ++ o ++ o''' ++ ["-hide-all-packages"]

  where pkgName (Dependency n _) = n

