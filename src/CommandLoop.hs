{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DoAndIfThenElse       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module CommandLoop
    ( setupCommandLoop
    , CommandObj
    ) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Concurrent.Event        as Event
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Reader.Class
import           Control.Monad.State.Class
import           Control.Proxy
import           Control.Proxy.Concurrent
import           Control.Proxy.Trans.Reader      hiding (ask, asks, local)
import qualified Control.Proxy.Trans.Reader      as ReaderP
import           Control.Proxy.Trans.State       hiding (get, gets, modify, put,
                                                  state, stateT)
import qualified Control.Proxy.Trans.State       as StateP
import           Data.Foldable                   (traverse_)
import           Data.IORef
import           Data.List                       (find)
import           Data.Maybe
import qualified Data.Sequence                   as S
import           Distribution.PackageDescription (allBuildInfo, hsSourceDirs)
import           Distribution.ParseUtils
import qualified ErrUtils
import qualified FastString                      as GHC
import qualified GHC
import qualified GHC.Paths
import qualified MonadUtils                      as GHC
import qualified Outputable
import qualified SrcLoc                          as GHC
import           System.Directory                (canonicalizePath)
import           System.Exit                     (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath                 (makeRelative, takeDirectory)
import           System.Posix.Files
import           System.Posix.Signals
import           System.Posix.Types

import           Cabal
import           Info                            (getIdentifierInfo, getType)
import           Types

-- Orphan
instance (Proxy p, Monad m) => MonadState s (StateP s p a' a b' b m) where
  get = StateP.get
  put = StateP.put

instance (Proxy p, Monad m) => MonadReader s (ReaderP s p a' a b' b m) where
  ask = ReaderP.ask
  local = ReaderP.local

type CommandObj = (Command, [String])

data ErrorInfo = ErrorInfo
  { _severity       :: GHC.Severity
  , _location       :: GHC.SrcSpan
  , _style          :: Outputable.PprStyle
  , _messageChanges :: String -> String
  , _message        :: GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> String
  }
makeLenses ''ErrorInfo

data Settings = Settings
  { _cabalFile          :: Maybe FilePath
  , _refWarningsEnabled :: IORef Bool
  , _refFileName        :: IORef GHC.FastString
  , _refFilePath        :: IORef GHC.FastString
  , _refErrors          :: IORef (S.Seq ErrorInfo)
  , _extraOptions       :: [String]
  }
makeLenses ''Settings

data Options = Options
  { _cabalMod     :: EpochTime
  , _ghcOpts      :: [String]
  , _needReconfig :: Bool
  }
makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options
  { _cabalMod = 0
  , _ghcOpts = []
  , _needReconfig = True
  }

readRef :: (GHC.MonadIO m, MonadTrans t, MonadReader s (t m)) => Lens' s (IORef a) -> t m a
readRef l = view l >>= lift . GHC.liftIO . readIORef

writeRef :: (GHC.MonadIO m, MonadTrans t, MonadReader s (t m)) => Lens' s (IORef a) -> a -> t m ()
writeRef l v = view l >>= lift . GHC.liftIO . flip writeIORef v

commandLoop :: (Proxy p) => () -> Pipe (StateP Options (ReaderP Settings p)) CommandObj ClientDirective GHC.Ghc ()
commandLoop () = do
  let status = respond . ClientLog "CommandLoop"
  forever $ do
    unitD ->> reconfigure
    status "Process commands"
    (request () >>= (unitD ->>) . processCommandObj) `untilM` use needReconfig

reconfigure :: (Proxy p) => Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
reconfigure = do
  let status = respond . ClientLog "Reconfigure"
  status "Updating GHC options"
  opts <- use ghcOpts
  settings <- liftP ask
  (configOk,errors) <- lift $ GHC.gcatch (newSession (settings ^. refErrors) (settings ^. extraOptions ++ opts) >> return (True,[])) handleConfigError
  status "Processing configure result"
  needReconfig .= not configOk
  unless configOk $ traverse_ respond errors

processCommandObj :: (Proxy p) => CommandObj -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
processCommandObj (command,opts') = do
  let logmsg = respond . ClientLog "processCommandObj"
  logmsg $ "Processing command: " ++ show command
  opts <- use ghcOpts
  when (opts /= opts') $ do
    logmsg "Reconfigure needed"
    needReconfig .= True
  liftP $ writeRef refErrors S.empty
  result <- runCommand command
  logmsg "Finished executing command. Start collecting errors"
  errors <- liftP $ readRef refErrors
  logmsg "Responding with errors"
  traverse_ processError errors
  logmsg "Done. Returning ClientExit status"
  respond $ ClientExit result
  return ()

renderError :: ErrorInfo -> String
renderError err = view message err (err ^. severity) (err ^. location) (err ^. style)

isWarning :: GHC.Severity -> Bool
isWarning GHC.SevWarning = True
isWarning _ = False

processError :: (Proxy p) => ErrorInfo -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
processError err = liftP $ hoist GHC.liftIO $ do
  name <- lift . readIORef =<< view refFileName
  path <- lift . readIORef =<< view refFilePath
  warningsEnabled <- lift . readIORef =<< view refWarningsEnabled
  let msg = renderError $ adjustFileName name path err
  when (warningsEnabled || not (isWarning $ err ^. severity)) $
    respond $ ClientStdout msg

setupCommandLoop :: Maybe FilePath -> [String] -> IO (Input CommandObj, Output ClientDirective, Input ClientDirective)
setupCommandLoop cabal initialGhcOpts = do
  (serverInput,serverOutput) <- spawn Unbounded
  (clientInput,clientOutput) <- spawn Unbounded
  warnings <- newIORef True
  fileName <- newIORef $ GHC.fsLit ""
  filePath <- newIORef $ GHC.fsLit ""
  errors <- newIORef S.empty
  cabalOptions <- cabalMiscOptions
  let settings = Settings cabal warnings fileName filePath errors cabalOptions

  finishGHCstartup <- Event.new

  _ <- forkIO $ do
    GHC.runGhc (Just GHC.Paths.libdir) $ do
       GHC.liftIO $ Event.signal finishGHCstartup
       runProxy $ runReaderK settings $ evalStateK (defaultOptions & ghcOpts .~ initialGhcOpts) $
         hoist GHC.liftIO . recvS clientOutput >-> commandLoop >-> hoist GHC.liftIO . sendD serverInput

  t <- myThreadId
  -- GHC for some reason changes the default ^C handlers. They don't work when used in a thread, so we reset
  -- them here, after GHC started so we're sure to override GHC's handler.
  Event.wait finishGHCstartup
  _ <- installHandler sigINT (Catch $ throwTo t UserInterrupt) Nothing
  _ <- installHandler sigTERM (Catch $ throwTo t UserInterrupt) Nothing

  return (clientInput,serverOutput,serverInput)

newSession :: IORef (S.Seq ErrorInfo) -> [String] -> GHC.Ghc ()
newSession errorIn opts = do
  initialDynFlags <- GHC.getSessionDynFlags
  let updatedDynFlags = initialDynFlags
       { GHC.log_action = logAction' errorIn
       , GHC.ghcLink = GHC.NoLink
       , GHC.hscTarget = GHC.HscInterpreted
       }
  (finalDynFlags, _, _) <- GHC.parseDynamicFlags updatedDynFlags (map GHC.noLoc opts)
  void $ GHC.setSessionDynFlags finalDynFlags


handleConfigError :: GHC.GhcException -> GHC.Ghc (Bool, [ClientDirective])
handleConfigError e = return $ (False,)
  [ ClientStderr (GHC.showGhcException e "")
  , ClientExit (ExitFailure 1)
  ]

doMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
doMaybe Nothing _ = return ()
doMaybe (Just x) f = f x

cabalCached :: (MonadTrans t, MonadState Options (t m), GHC.GhcMonad m) => FilePath -> t m () -> t m ()
cabalCached file action = do
  s <- lift $ GHC.liftIO $ getSymbolicLinkStatus file
  let tnew = modificationTime s
  told <- use cabalMod
  when (told < tnew) $ do
    cabalMod .= tnew
    action

setCabalPerFileOpts :: (Proxy p) => FilePath -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
setCabalPerFileOpts configPath = cabalCached configPath $ do
  respond $ ClientLog "CabalFileOpts" "Loading cabal"
  file <- fmap GHC.unpackFS $ liftP $ readRef refFileName
  cabalDir <- lift $ GHC.liftIO $ canonicalizePath $ takeDirectory configPath
  fileRel <- lift $ GHC.liftIO $ makeRelative cabalDir <$> canonicalizePath file
  config <- lift $ GHC.liftIO $ readFile configPath
  case parseCabalConfig config of
    (ParseFailed _) -> respond $ ClientStderr "Warning: Parsing of the cabal config failed. Please correct it, or use --no-cabal."
    (ParseOk _ r) -> do
      let srcInclude = "-i" ++ takeDirectory file
      let opts = fromMaybe [] $ getBuildInfoOptions =<< findBuildInfoFile r fileRel
      respond $ ClientLog "CabalFileOpts" $ "Cabal: Added options " ++ show opts
      dynFlags <- lift GHC.getSessionDynFlags
      (finalDynFlags, _, _) <- lift $ GHC.parseDynamicFlags dynFlags (map GHC.noLoc $ srcInclude : opts)
      void $ lift $ GHC.setSessionDynFlags finalDynFlags

setAllCabalImportDirs :: (Proxy p) => FilePath -> Producer p ClientDirective GHC.Ghc ()
setAllCabalImportDirs cabal = runIdentityP $ do
  config <- lift $ GHC.liftIO $ readFile cabal
  case parseCabalConfig config of
    (ParseFailed _) -> return ()
    (ParseOk _ r) -> do
      let dirs = allBuildInfo r >>= hsSourceDirs
      respond $ ClientLog "setAllCabalImportDirs" $ "Added directories: " ++ show dirs
      dynFlags <- lift GHC.getSessionDynFlags
      (finalDynFlags,_,_) <- lift $ GHC.parseDynamicFlags dynFlags (map (GHC.noLoc . ("-i" ++)) dirs)
      void $ lift $ GHC.setSessionDynFlags finalDynFlags

setCurrentFile :: (Proxy p) => FilePath -> FilePath -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
setCurrentFile path name = liftP $ do
  writeRef refFileName $ GHC.fsLit name
  writeRef refFilePath $ GHC.fsLit path

withWarnings :: (Proxy p) => Bool -> GHC.Ghc a -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc a
withWarnings v action = do
  beforeValue <- liftP $ readRef refWarningsEnabled
  ref <- liftP $ view refWarningsEnabled
  liftP $ writeRef refWarningsEnabled v
  lift (action `GHC.gfinally` GHC.liftIO (writeIORef ref beforeValue))

runCommand :: (Proxy p) => Command -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ExitCode
runCommand (CmdCheck real file) = do
    let status = respond . ClientLog "Check"
    status "Initalize state and cabal"
    setCurrentFile real file
    let noPhase = Nothing
    liftP (view cabalFile) >>= flip doMaybe setCabalPerFileOpts
    status "Load target"
    target <- lift $ GHC.guessTarget file noPhase
    lift $ GHC.setTargets [target]
    let handler err = GHC.printException err >> return GHC.Failed
    status "Compile target"
    flag <- lift $ GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
    status "Check result"
    case flag of
        GHC.Succeeded -> return ExitSuccess
        GHC.Failed -> return $ ExitFailure 1
runCommand (CmdModuleFile moduleName) = do
    let status = respond . ClientLog "ModuleFile"
    status "Initialize cabal"
    liftP (view cabalFile) >>= flip doMaybe setAllCabalImportDirs
    status "Load target"
    target <- lift $ GHC.guessTarget moduleName Nothing
    lift $ GHC.setTargets [target]
    -- TODO: This currently fails, need to figure out why (when cabal support enabled)
    status "Build module graph"
    moduleGraph <- lift $ GHC.depanal [] False `GHC.gcatch` \(SomeException _) -> return []
    status "Find module"
    case find (moduleSummaryMatchesModuleName moduleName) moduleGraph of
        Nothing -> do
          respond $ ClientStderr "Module not found"
          return $ ExitFailure 1
        Just modSummary ->
            case GHC.ml_hs_file (GHC.ms_location modSummary) of
                Nothing -> do
                  respond $ ClientStderr "Module does not have a source file"
                  return $ ExitFailure 1
                Just file -> do
                  respond $ ClientStdout file
                  return ExitSuccess
    where
    moduleSummaryMatchesModuleName modName modSummary =
        modName == (GHC.moduleNameString . GHC.moduleName . GHC.ms_mod) modSummary
runCommand (CmdInfo real file identifier) = do
    let status = respond . ClientLog "Info"
    status "Initialize state and cabal"
    setCurrentFile real file
    liftP (view cabalFile) >>= flip doMaybe setCabalPerFileOpts
    status "Get info"
    result <- withWarnings False $ getIdentifierInfo file identifier
    status "Check result"
    case result of
        Left err -> do
          respond $ ClientStderr err
          return $ ExitFailure 1
        Right info -> do
           respond $ ClientStdout info
           return ExitSuccess
runCommand (CmdType real file (line, col)) = do
    let status = respond . ClientLog "Type"
    status "Initialize state and cabal"
    setCurrentFile real file
    liftP (view cabalFile) >>= flip doMaybe setCabalPerFileOpts
    status "Get type"
    result <- withWarnings False $
        getType file (line, col)
    status "Check result"
    case result of
        Left err -> do
          respond $ ClientStderr err
          return $ ExitFailure 1
        Right types -> do
            mapM_ (respond . ClientStdout . formatType) types
            return ExitSuccess
    where
    formatType :: ((Int, Int, Int, Int), String) -> String
    formatType ((startLine, startCol, endLine, endCol), t) =
        concat
            [ show startLine , " "
            , show startCol , " "
            , show endLine , " "
            , show endCol , " "
            , "\"", t, "\""
            ]

adjustFileName :: GHC.FastString -> GHC.FastString -> ErrorInfo -> ErrorInfo
adjustFileName fname fpath info = case info ^. location of
  (GHC.UnhelpfulSpan _) -> inOtherFile ""
  (GHC.RealSrcSpan span') -> if GHC.srcSpanFile span' == fpath then info & location .~ fileSrcSpan span' else inOtherFile $ GHC.unpackFS fname
  where inOtherFile f = info & messageChanges . mapped %~ ((f ++ ": ") ++)
                             & location .~ firstLineSpan
        firstLineSpan = GHC.RealSrcSpan $ GHC.mkRealSrcSpan (GHC.mkRealSrcLoc fname 1 1) (GHC.mkRealSrcLoc fname 1 1)
        fileSrcSpan span' = GHC.RealSrcSpan $ GHC.mkRealSrcSpan (GHC.mkRealSrcLoc fpath lineStart colStart) (GHC.mkRealSrcLoc fpath lineEnd colEnd)
          where colStart = GHC.srcSpanStartCol span'
                colEnd = GHC.srcSpanEndCol span'
                lineStart = GHC.srcSpanStartLine span'
                lineEnd = GHC.srcSpanEndLine span'

#if __GLASGOW_HASKELL__ >= 706

logAction' :: IORef (S.Seq ErrorInfo) -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction' errorIn dflags sev sspan mstyle doc =
  modifyIORef errorIn $ (S.|> ErrorInfo sev sspan mstyle id f)
  where f sev' span' = Outputable.renderWithStyle dflags (ErrUtils.mkLocMessage sev' span' doc)

#else

logAction' :: IORef (S.Seq ErrorInfo) -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO ()
logAction' errorIn sev sspan mstyle doc = modifyIORef errorIn $ (S.|> ErrorInfo sev sspan mstyle id f)
  where f sev' span' = Outputable.renderWithStyle (ErrUtils.mkLocMessage span' doc)


#endif
