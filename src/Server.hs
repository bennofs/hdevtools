module Server where

import           CommandLoop          (newCommandLoopState, startCommandLoop)
import           Control.Exception    (bracket, handleJust)
import           Control.Monad        (guard)
import           Control.Monad.IfElse (whenM)
import           Data.IORef           (IORef, newIORef, readIORef, writeIORef)
import           GHC.IO.Exception     (IOErrorType (ResourceVanished))
import           Network              (PortID (UnixSocket), Socket, accept,
                                       listenOn, sClose)
import           System.Directory     (doesFileExist, removeFile)
import           System.Exit          (ExitCode (ExitSuccess))
import           System.IO            (Handle, hClose, hFlush, hGetLine, hPrint)
import           System.IO.Error      (ioeGetErrorType)
import           Types                (ClientDirective (..), Command,
                                       ServerDirective (..))
import           Util                 (readMaybe)

withSocket :: FilePath -> (Socket -> IO a) -> IO a
withSocket sock = bracket (createListenSocket sock) (cleanupSocket sock)

cleanupSocket :: FilePath -> Socket -> IO ()
cleanupSocket sockFile sock = do
  sClose sock
  whenM (doesFileExist sockFile) $ removeFile sockFile


createListenSocket :: FilePath -> IO Socket
createListenSocket socketPath =
    listenOn (UnixSocket socketPath)

startServer :: Maybe FilePath -> Socket -> IO ()
startServer cabal sock = do
  state <- newCommandLoopState
  currentClient <- newIORef Nothing
  startCommandLoop cabal state (clientSend currentClient) (getNextCommand currentClient sock) [] Nothing

clientSend :: IORef (Maybe Handle) -> ClientDirective -> IO ()
clientSend currentClient clientDirective = do
    mbH <- readIORef currentClient
    case mbH of
        Just h -> ignoreEPipe $ do
            hPrint h clientDirective
            hFlush h
        Nothing -> error "This is impossible"
    where
    -- EPIPE means that the client is no longer there.
    ignoreEPipe = handleJust (guard . isEPipe) (const $ return ())
    isEPipe = (==ResourceVanished) . ioeGetErrorType

getNextCommand :: IORef (Maybe Handle) -> Socket -> IO (Maybe (Command, [String]))
getNextCommand currentClient sock = do
    checkCurrent <- readIORef currentClient
    case checkCurrent of
        Just h -> hClose h
        Nothing -> return ()
    (h, _, _) <- accept sock
    writeIORef currentClient (Just h)
    msg <- hGetLine h -- TODO catch exception
    let serverDirective = readMaybe msg
    case serverDirective of
        Nothing -> do
            clientSend currentClient $ ClientUnexpectedError $
                "The client sent an invalid message to the server: " ++ show msg
            getNextCommand currentClient sock
        Just (SrvCommand cmd ghcOpts) -> return $ Just (cmd, ghcOpts)
        Just SrvStatus -> do
            mapM_ (clientSend currentClient)
                [ ClientStdout "Server is running."
                , ClientExit ExitSuccess
                ]
            getNextCommand currentClient sock
        Just SrvExit -> do
            mapM_ (clientSend currentClient)
                [ ClientStdout "Shutting down server."
                , ClientExit ExitSuccess
                ]
            -- Must close the handle here because we are exiting the loop so it
            -- won't be closed in the code above
            hClose h
            return Nothing
