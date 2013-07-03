module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import           Control.Exception (finally, tryJust)
import           Control.Monad     (guard, when)
import           Daemonize         (daemonize)
import           Network           (PortID (UnixSocket), connectTo)
import           Server            (cleanupSocket, createListenSocket,
                                    startServer)
import           System.Exit       (exitFailure, exitWith)
import           System.IO         (Handle, hClose, hFlush, hGetLine, hPrint,
                                    hPutStrLn, stderr)
import           System.IO.Error   (isDoesNotExistError)
import           Types             (ClientDirective (..), Command (..),
                                    ServerDirective (..))
import           Util              (readMaybe)

connect :: FilePath -> IO Handle
connect = connectTo "" . UnixSocket

getServerStatus :: Bool -> FilePath -> IO ()
getServerStatus verbose sock = do
    h <- connect sock
    hPrint h SrvStatus
    hFlush h
    startClientReadLoop verbose h

stopServer :: Bool -> FilePath -> IO ()
stopServer verbose sock = do
    h <- connect sock
    hPrint h SrvExit
    hFlush h
    startClientReadLoop verbose h

serverCommand :: Bool -> Maybe FilePath -> FilePath -> Command -> [String] -> IO ()
serverCommand verbose cabal sock cmd ghcOpts = do
    r <- tryJust (guard . isDoesNotExistError) (connect sock)
    case r of
        Right h -> do
            hPrint h $ SrvCommand cmd ghcOpts
            hFlush h
            startClientReadLoop verbose h
        Left _ -> do
             s <- createListenSocket sock
             daemonize False $ startServer cabal s `finally` cleanupSocket sock s
             serverCommand verbose cabal sock cmd ghcOpts

startClientReadLoop :: Bool -> Handle -> IO ()
startClientReadLoop verbose h = do
    message <- hGetLine h
    let clientDirective = readMaybe message
    case clientDirective of
        Just (ClientStdout out) -> putStrLn out >> putStrLn "" >> startClientReadLoop verbose h
        Just (ClientStderr err) -> hPutStrLn stderr err >> putStrLn "" >> startClientReadLoop verbose h
        Just (ClientLog loc msg) -> when verbose (putStrLn $ "LOG [" ++ loc ++ "] " ++ msg) >> startClientReadLoop verbose h
        Just (ClientExit exitCode) -> hClose h >> exitWith exitCode
        Just (ClientUnexpectedError err) -> hClose h >> unexpectedError err
        Nothing -> do
            hClose h
            unexpectedError $ "The server sent an invalid message to the client: " ++ show message

unexpectedError :: String -> IO ()
unexpectedError err = do
    hPutStrLn stderr banner
    hPutStrLn stderr err
    hPutStrLn stderr banner
    exitFailure
    where banner = replicate 78 '*'
