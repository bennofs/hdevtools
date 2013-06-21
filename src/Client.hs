module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import           Control.Exception (tryJust)
import           Control.Monad     (guard)
import           Network           (PortID (UnixSocket), connectTo)
import           System.Exit       (exitFailure, exitWith)
import           System.IO         (Handle, hClose, hFlush, hGetLine, hPrint,
                                    hPutStrLn, stderr)
import           System.IO.Error   (isDoesNotExistError)

import           Daemonize         (daemonize)
import           Server            (startServer, withSocket)
import           Types             (ClientDirective (..), Command (..),
                                    ServerDirective (..))
import           Util              (readMaybe)

connect :: FilePath -> IO Handle
connect = connectTo "" . UnixSocket

getServerStatus :: FilePath -> IO ()
getServerStatus sock = do
    h <- connect sock
    hPrint h SrvStatus
    hFlush h
    startClientReadLoop h

stopServer :: FilePath -> IO ()
stopServer sock = do
    h <- connect sock
    hPrint h SrvExit
    hFlush h
    startClientReadLoop h

serverCommand :: Maybe FilePath -> FilePath -> Command -> [String] -> IO ()
serverCommand cabal sock cmd ghcOpts = do
    r <- tryJust (guard . isDoesNotExistError) (connect sock)
    case r of
        Right h -> do
            hPrint h $ SrvCommand cmd ghcOpts
            hFlush h
            startClientReadLoop h
        Left _ -> do
            daemonize False $ withSocket sock $ startServer cabal
            serverCommand cabal sock cmd ghcOpts

startClientReadLoop :: Handle -> IO ()
startClientReadLoop h = do
    msg <- hGetLine h
    let clientDirective = readMaybe msg
    case clientDirective of
        Just (ClientStdout out) -> putStrLn out >> putStrLn "" >> startClientReadLoop h
        Just (ClientStderr err) -> hPutStrLn stderr err >> putStrLn "" >> startClientReadLoop h
        Just (ClientExit exitCode) -> hClose h >> exitWith exitCode
        Just (ClientUnexpectedError err) -> hClose h >> unexpectedError err
        Nothing -> do
            hClose h
            unexpectedError $
                "The server sent an invalid message to the client: " ++ show msg

unexpectedError :: String -> IO ()
unexpectedError err = do
    hPutStrLn stderr banner
    hPutStrLn stderr err
    hPutStrLn stderr banner
    exitFailure
    where banner = replicate 78 '*'
