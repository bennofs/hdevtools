module Main where

import           Control.Applicative    ((<$>))
import           Control.Monad          (filterM)
import           Data.List              (find, isSuffixOf)
import           System.Directory       (canonicalizePath, doesDirectoryExist,
                                         doesFileExist, getDirectoryContents)
import           System.Environment     (getProgName)
import           System.FilePath        (takeDirectory, takeFileName, (</>))
import           System.IO              (hPutStrLn, stderr)
import           System.Posix.Directory (changeWorkingDirectory)

import           Client                 (getServerStatus, serverCommand,
                                         stopServer)
import           CommandArgs
import           Daemonize              (daemonize)
import           Server                 (startServer, withSocket)
import           Types                  (Command (..))

defaultSocketFilename :: FilePath
defaultSocketFilename = ".hdevtools.sock"

getSocketFilename :: Maybe FilePath -> FilePath
getSocketFilename Nothing = defaultSocketFilename
getSocketFilename (Just f) = f

findCabalConfig :: FilePath -> IO (Maybe FilePath)
findCabalConfig d = do
  de <- doesDirectoryExist d
  d' <- canonicalizePath d
  d'' <- canonicalizePath (d </> "..")
  if d' == d''
    then return Nothing
    else do
  if not de
     then return Nothing
     else do
  files <- filterM doesFileExist . map (d </>) =<< getDirectoryContents d
  case find (".cabal" `isSuffixOf`) files of
    Nothing -> findCabalConfig $ d </> ".."
    (Just f) -> return (Just f)

main :: IO ()
main = do
    args <- loadHDevTools
    cabal <- if no_cabal args then return Nothing else findCabalConfig "."
    startDir <- canonicalizePath "."
    case cabal of
      Just f -> changeWorkingDirectory $ takeDirectory f
      Nothing -> return ()
    let cabalf = takeFileName <$> cabal
    let sock = getSocketFilename (socket args)
    case args of
        Admin {} -> doAdmin cabalf sock args
        Check {} -> doCheck startDir cabalf sock args
        ModuleFile {} -> doModuleFile cabalf sock args
        Info {} -> doInfo startDir cabalf sock args
        Type {} -> doType startDir cabalf sock args

doAdmin :: Maybe FilePath -> FilePath -> HDevTools -> IO ()
doAdmin cabal sock args
    | start_server args =
        (if noDaemon args then id else daemonize True) $ withSocket sock $ startServer cabal
    | status args = getServerStatus (verbose args) sock
    | stop_server args = stopServer (verbose args) sock
    | otherwise = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a command. See:"
        hPutStrLn stderr $ progName ++ " --help"

doModuleFile :: Maybe FilePath -> FilePath -> HDevTools -> IO ()
doModuleFile cabal sock args =
    serverCommand (verbose args) cabal sock (CmdModuleFile (module_ args)) (ghcOpts args)

doFileCommand :: FilePath -> Maybe FilePath -> String -> (FilePath -> HDevTools -> Command) -> FilePath -> HDevTools -> IO ()
doFileCommand startDir cabal cmdName cmd sock args
    | null (file args) = do
        progName <- getProgName
        hPutStrLn stderr "You must provide a haskell source file. See:"
        hPutStrLn stderr $ progName ++ " " ++ cmdName ++ " --help"
    | otherwise = serverCommand (verbose args) cabal sock (cmd (file args) $ args {file = startDir </> file args}) (ghcOpts args)

doCheck :: FilePath -> Maybe FilePath -> FilePath -> HDevTools -> IO ()
doCheck startDir cabal = doFileCommand startDir cabal "check" $
    \real args -> CmdCheck real (file args)

doInfo :: FilePath -> Maybe FilePath -> FilePath -> HDevTools -> IO ()
doInfo startDir cabal = doFileCommand startDir cabal "info" $
    \real args -> CmdInfo real (file args) (identifier args)

doType :: FilePath -> Maybe FilePath -> FilePath -> HDevTools -> IO ()
doType startDir cabal = doFileCommand startDir cabal "type" $
    \real args -> CmdType real (file args) (line args, col args)
