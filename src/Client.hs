module Client
    ( getServerStatus
    , stopServer
    , serverCommand
    ) where

import Control.Concurrent
import Control.Exception    (finally, tryJust)
import Control.Monad        (guard, when)
import Control.Monad.IfElse (whenM)
import Control.Monad.Loops
import Daemonize            (daemonize)
import Network              (PortID (UnixSocket), connectTo)
import Server               (cleanupSocket, createListenSocket,
                                       startServer)
import System.Directory     (doesFileExist, removeFile)
import System.Exit          (exitFailure, exitWith)
import System.IO            (Handle, hClose, hFlush, hGetLine, hPrint,
                                       hPutStrLn, stderr, stdout)
import System.IO.Error      (isDoesNotExistError)
import Types                (ClientDirective (..), Command (..),
                                       ServerDirective (..))
import Util                 (readMaybe)

connect :: FilePath -> IO Handle
connect = connectTo "" . UnixSocket

getServerStatus :: Bool -> FilePath -> IO ()
getServerStatus verbose sock = do
    h <- connect sock
    hPrint h SrvStatus
    hFlush h
    startClientReadLoop verbose h True

stopServer :: Bool -> FilePath -> IO ()
stopServer verbose sock = do
    h <- connect sock
    hPrint h SrvExit
    hFlush h
    startClientReadLoop verbose h False
    putStr "Waiting for server to exit ... "
    hFlush stdout
    whileM_ (doesFileExist sock) $ threadDelay 100000 -- 100 ms
    putStrLn "Done"

serverCommand :: Bool -> Maybe FilePath -> FilePath -> Command -> [String] -> IO ()
serverCommand verbose cabal sock cmd ghcOpts = do
    r <- tryJust (guard . isDoesNotExistError) (connect sock)
    case r of
        Right h -> do
            hPrint h $ SrvCommand cmd ghcOpts
            hFlush h
            startClientReadLoop verbose h True
        Left _ -> do
             whenM (doesFileExist sock) $ removeFile sock
             s <- createListenSocket sock
             daemonize False $ startServer cabal s `finally` cleanupSocket sock s
             serverCommand verbose cabal sock cmd ghcOpts

startClientReadLoop :: Bool -> Handle -> Bool -> IO ()
startClientReadLoop verbose h exit = do
    message <- hGetLine h
    let clientDirective = readMaybe message
    case clientDirective of
        Just (ClientStdout out) -> putStrLn out >> putStrLn "" >> startClientReadLoop verbose h exit
        Just (ClientStderr err) -> hPutStrLn stderr err >> putStrLn "" >> startClientReadLoop verbose h exit
        Just (ClientLog loc msg) -> when verbose (putStrLn $ "LOG [" ++ loc ++ "] " ++ msg) >> startClientReadLoop verbose h exit
        Just (ClientExit exitCode) -> hClose h >> when exit (exitWith exitCode)
        Just (ClientUnexpectedError err) -> hClose h >> unexpectedError err exit
        Nothing -> do
            hClose h
            flip unexpectedError exit $ "The server sent an invalid message to the client: " ++ show message

unexpectedError :: String -> Bool -> IO ()
unexpectedError err exit = do
    hPutStrLn stderr banner
    hPutStrLn stderr err
    hPutStrLn stderr banner
    when exit exitFailure
    where banner = replicate 78 '*'
