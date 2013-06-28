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
    ) where

import           Control.Concurrent
import           Control.Exception               (SomeException (..))
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
import           Data.IORef
import           Data.List                       (find)
import           Data.Maybe
import           Distribution.PackageDescription (allBuildInfo, hsSourceDirs)
import           Distribution.ParseUtils
import qualified ErrUtils
import qualified FastString                      as GHC
import qualified GHC
import qualified GHC.Paths
import           MonadUtils                      (MonadIO, liftIO)
import qualified MonadUtils                      as GHC
import qualified Outputable
import qualified SrcLoc                          as GHC
import           System.Directory                (makeRelativeToCurrentDirectory)
import           System.Exit                     (ExitCode (ExitFailure, ExitSuccess))
import           System.FilePath                 (takeDirectory)
import           System.Posix.Files
import           System.Posix.Types

import           Cabal
import           Info                            (getIdentifierInfo, getType)
import           Types

-- Orphan
instance (Proxy p, Monad m) => MonadState s (StateP s p a' a b' b m) where
  get = StateP.get
  put = StateP.put
  state = StateP.state

instance (Proxy p, Monad m) => MonadReader s (ReaderP s p a' a b' b m) where
  ask = ReaderP.ask
  local = ReaderP.local
  reader = fmap ?? ask

type CommandObj = (Command, [String])

data Settings = Settings
  { _cabalFile          :: Maybe FilePath
  , _refWarningsEnabled :: IORef Bool
  , _refFileName        :: IORef GHC.FastString
  , _refFilePath        :: IORef GHC.FastString
  }
makeLenses ''Settings

data Options = Options
  { _cabalMod     :: EpochTime
  , _ghcOpts      :: [String]
  , _needReconfig :: Bool
  }
makeLenses ''Options

data ErrorInfo = ErrorInfo
  { _severity       :: GHC.Severity
  , _location       :: GHC.SrcSpan
  , _style          :: Outputable.PprStyle
  , _messageChanges :: String -> String
  , _message        :: GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> String
  }
makeLenses ''ErrorInfo

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

commandLoop :: (Proxy p) => Input ErrorInfo -> () -> Pipe (StateP Options (ReaderP Settings p)) CommandObj ClientDirective IO ()
commandLoop errorIn () = do
  cabalOpts <- lift cabalMiscOptions
  void $ hoist (liftIO . GHC.runGhc (Just GHC.Paths.libdir)) $ forever $ do
      opts <- use ghcOpts
      (configOk,errors) <- lift $ GHC.gcatch (newSession errorIn (cabalOpts ++ opts) >> return (True,[])) handleConfigError
      unless configOk $ traverse respond errors >> needReconfig .= True
      (request () >>= (unitD ->>) . processCommandObj) `untilM` use needReconfig

  return ()

processCommandObj :: (Proxy p) => CommandObj -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
processCommandObj (command,opts') = do
  opts <- use ghcOpts
  if opts /= opts'
    then do
      ghcOpts .= opts'
      needReconfig .= True
    else runCommand command


renderError :: ErrorInfo -> String
renderError err = view message err (err ^. severity) (err ^. location) (err ^. style)

isWarning :: GHC.Severity -> Bool
isWarning GHC.SevWarning = True
isWarning _ = False

processError :: (Proxy p) => () -> Pipe (ReaderP Settings p) ErrorInfo ClientDirective IO ()
processError () = forever $ do
  err <- request ()
  name <- lift . readIORef =<< view refFileName
  path <- lift . readIORef =<< view refFilePath
  warningsEnabled <- lift . readIORef =<< view refWarningsEnabled
  let msg = renderError $ adjustFileName name path err
  when (warningsEnabled || not (isWarning $ err ^. severity)) $
    respond $ ClientStdout msg


setupCommandLoop :: (Proxy p, MonadIO m) => Maybe FilePath -> [String] -> IO (Input CommandObj, Output ClientDirective)
setupCommandLoop cabal initialGhcOpts = do
  (serverInput,serverOutput) <- spawn Unbounded
  (clientInput,clientOutput) <- spawn Unbounded
  (errorIn, errorOut) <- spawn Unbounded
  warnings <- newIORef True
  fileName <- newIORef $ GHC.fsLit ""
  filePath <- newIORef $ GHC.fsLit ""

  _ <- forkIO $ do
    runProxy $ runReaderK (Settings cabal warnings fileName filePath) $ evalStateK (defaultOptions & ghcOpts .~ initialGhcOpts) $
      recvS clientOutput >-> commandLoop errorIn  >-> sendD serverInput
    performGC

  _ <- forkIO $ do
    runProxy $ runReaderK (Settings cabal warnings fileName filePath) $ recvS errorOut >-> processError >-> sendD serverInput
    performGC

  return (clientInput,serverOutput)

newSession :: Input ErrorInfo -> [String] -> GHC.Ghc ()
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
  respond $ ClientLog "Loading cabal"
  file <- fmap GHC.unpackFS $ liftP $ readRef refFileName
  f <- lift $ GHC.liftIO $ makeRelativeToCurrentDirectory file
  config <- lift $ GHC.liftIO $ readFile configPath
  case parseCabalConfig config of
    (ParseFailed _) -> respond $ ClientStderr "Warning: Parsing of the cabal config failed. Please correct it, or use --no-cabal."
    (ParseOk _ r) -> do
      let srcInclude = "-i" ++ takeDirectory file
      let opts = fromMaybe [] $ getBuildInfoOptions =<< findBuildInfoFile r f
      dynFlags <- lift GHC.getSessionDynFlags
      (finalDynFlags, _, _) <- lift $ GHC.parseDynamicFlags dynFlags (map GHC.noLoc $ srcInclude : opts)
      void $ lift $ GHC.setSessionDynFlags finalDynFlags

setAllCabalImportDirs :: (Proxy p) => FilePath -> Producer p a GHC.Ghc ()
setAllCabalImportDirs cabal = runIdentityP $ do
  config <- lift $ GHC.liftIO $ readFile cabal
  case parseCabalConfig config of
    (ParseFailed _) -> return ()
    (ParseOk _ r) -> do
      let dirs = allBuildInfo r >>= hsSourceDirs
      dynFlags <- lift GHC.getSessionDynFlags
      (finalDynFlags,_,_) <- lift $ GHC.parseDynamicFlags dynFlags (map (GHC.noLoc . ("-i" ++)) dirs)
      void $ lift $ GHC.setSessionDynFlags finalDynFlags

setCurrentFile :: (Proxy p) => FilePath -> FilePath -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
setCurrentFile name path = liftP $ do
  writeRef refFileName $ GHC.fsLit name
  writeRef refFilePath $ GHC.fsLit path

withWarnings :: (Proxy p) => Bool -> GHC.Ghc a -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc a
withWarnings v action = do
  beforeValue <- liftP $ readRef refWarningsEnabled
  ref <- liftP $ view refWarningsEnabled
  liftP $ writeRef refWarningsEnabled v
  lift (action `GHC.gfinally` GHC.liftIO (writeIORef ref beforeValue))

runCommand :: (Proxy p) => Command -> Producer (StateP Options (ReaderP Settings p)) ClientDirective GHC.Ghc ()
runCommand (CmdCheck real file) = do
    respond $ ClientLog "[Check] Initalize state"
    setCurrentFile real file
    respond $ ClientLog "[Check] Load cabal"
    let noPhase = Nothing
    liftP (view cabalFile) >>= flip doMaybe setCabalPerFileOpts
    respond $ ClientLog "[Check] Load target"
    target <- lift $ GHC.guessTarget file noPhase
    lift $ GHC.setTargets [target]
    let handler err = GHC.printException err >> return GHC.Failed
    respond $ ClientLog "[Check] Compile target"
    flag <- lift $ GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
    respond $ ClientLog "[Check] Send result"
    case flag of
        GHC.Succeeded -> respond $ ClientExit ExitSuccess
        GHC.Failed -> respond $ ClientExit (ExitFailure 1)
runCommand (CmdModuleFile moduleName) = do
    liftP (view cabalFile) >>= flip doMaybe setAllCabalImportDirs
    target <- lift $ GHC.guessTarget moduleName Nothing
    lift $ GHC.setTargets [target]
    -- TODO: This currently fails, need to figure out why (when cabal support enabled)
    moduleGraph <- lift $ GHC.depanal [] False `GHC.gcatch` \(SomeException _) -> return []
    case find (moduleSummaryMatchesModuleName moduleName) moduleGraph of
        Nothing -> do
          respond $ ClientStderr "Module not found"
          respond $ ClientExit (ExitFailure 1)
        Just modSummary ->
            case GHC.ml_hs_file (GHC.ms_location modSummary) of
                Nothing -> do
                  respond $ ClientStderr "Module does not have a source file"
                  respond $ ClientExit (ExitFailure 1)
                Just file -> do
                  respond $ ClientStdout file
                  respond $ ClientExit ExitSuccess
    where
    moduleSummaryMatchesModuleName modName modSummary =
        modName == (GHC.moduleNameString . GHC.moduleName . GHC.ms_mod) modSummary
runCommand (CmdInfo real file identifier) = do
    setCurrentFile real file
    liftP (view cabalFile) >>= flip doMaybe setCabalPerFileOpts
    result <- withWarnings False $ getIdentifierInfo file identifier
    case result of
        Left err -> do
          respond $ ClientStderr err
          respond $ ClientExit (ExitFailure 1)
        Right info -> do
           respond $ ClientStdout info
           respond $ ClientExit ExitSuccess
runCommand (CmdType real file (line, col)) = do
    setCurrentFile real file
    liftP (view cabalFile) >>= flip doMaybe setCabalPerFileOpts
    result <- withWarnings False $
        getType file (line, col)
    case result of
        Left err -> do
          respond $ ClientStderr err
          respond $ ClientExit (ExitFailure 1)
        Right types -> do
            mapM_ (respond . ClientStdout . formatType) types
            respond $ ClientExit ExitSuccess
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

logAction' :: Input ErrorInfo -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction' errorIn dflags sev sspan mstyle doc = void $ atomically $ send errorIn $ ErrorInfo sev sspan mstyle id f
  where f sev' span' = Outputable.renderWithStyle dflags (ErrUtils.mkLocMessage sev' span' doc)

#else

logAction' :: Input ErrorInfo -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO ()
logAction' errorIn sev sspan mstyle doc = void $ atomically $ send errorIn $ ErrorInfo sev sspan mstyle id f
  where f sev' span' = Outputable.renderWithStyle (ErrUtils.mkLocMessage span' doc)


#endif
