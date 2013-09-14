module Server where

import           Control.Concurrent.Async
import           Control.Exception         (bracket, handleJust)
import           Control.Monad             (guard, void, when)
import           Control.Monad.IfElse      (whenM)
import           Control.Monad.Loops       (iterateWhile)
import           Control.Proxy
import           Control.Proxy.Concurrent  (atomically, recvS, send, sendD)
import           Control.Proxy.Trans.Maybe
import           Data.Maybe
import           GHC.IO.Exception          (IOErrorType (ResourceVanished))
import qualified Network                   as N
import qualified Network.Socket            as N hiding (accept)
import           System.Directory          (doesFileExist, removeFile)
import           System.Exit               (ExitCode (ExitSuccess))
import           System.IO
import           System.IO.Error           (ioeGetErrorType)

import           CommandLoop
import           Types
import           Util

withSocket :: FilePath -> (N.Socket -> IO a) -> IO a
withSocket sock = bracket (createListenSocket sock) (cleanupSocket sock)

cleanupSocket :: FilePath -> N.Socket -> IO ()
cleanupSocket sockFile sock = do
  N.close sock
  whenM (doesFileExist sockFile) $ removeFile sockFile

createListenSocket :: FilePath -> IO N.Socket
createListenSocket socketPath = N.listenOn (N.UnixSocket socketPath)

acceptOne :: N.Socket -> (Handle -> IO a) -> IO a
acceptOne sock action = do
  (clientHandle,_,_) <- N.accept sock
  action clientHandle

ignoreEPipe :: a -> IO a -> IO a
ignoreEPipe d = handleJust (guard . isEPipe) (const $ return d)
  where isEPipe e = ioeGetErrorType e == ResourceVanished

processRequest :: (Proxy p) => () -> Pipe (MaybeP p) String (Either ClientDirective CommandObj) IO ()
processRequest () = forever $ do
  let logmsg = respond . Left . ClientLog "processRequest"
  logmsg "Waiting for requests"
  line <- request ()
  logmsg "Received request"
  case readMaybe line of
   Just (SrvCommand cmd ghcOpts) -> respond $ Right (cmd, ghcOpts)
   Just SrvStatus -> do
     respond $ Left $ ClientStdout "Server is running"
     respond $ Left $ ClientExit ExitSuccess
   Just SrvExit -> do
     respond $ Left $ ClientLog "processRequest" "Shutting server down"
     respond $ Left $ ClientExit ExitSuccess
     nothing
   Nothing ->
     respond $ Left $ ClientUnexpectedError $ "The client sent an invalid message to the server: " ++ line

isClientExit :: ClientDirective -> Bool
isClientExit (ClientExit _) = True
isClientExit _ = False

takeWhileInclusiveD :: (Proxy p, Monad m) => (a -> Bool) -> a' -> p a' a a' a m ()
takeWhileInclusiveD  p = runIdentityK go
  where go a' = do
          a <- request a'
          a'2 <- respond a
          when (p a) $ go a'2

startServer :: Maybe FilePath -> N.Socket -> IO ()
startServer cabal sock = do
  (inp,outp,rawi) <- setupCommandLoop cabal []
  void $ iterateWhile id $ acceptOne sock $ \clientHandle -> ignoreEPipe True $ do
    void $ atomically $ send rawi $ ClientLog "startServer" "New client connection"
    a <- async $ runProxy $ recvS outp >-> takeWhileInclusiveD (not . isClientExit) >-> mapD show >-> hPutStrLnD clientHandle
    r <- runProxy $ runMaybeK $ hGetLineS clientHandle >-> processRequest >-> leftD (sendD rawi) >-> rightD (sendD inp)
    do
      void $ atomically $ send rawi $ ClientLog "startServer" "Waiting for end of send process"
      wait a
    return (isJust r)
