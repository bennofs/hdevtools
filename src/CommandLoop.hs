{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module CommandLoop
    ( newCommandLoopState
    , startCommandLoop
    , setupCommandLoop
    ) where

import           Control.Applicative             ((<$>))
import           Control.Concurrent
import           Control.Exception               (SomeException (..))
import           Control.Lens
import           Control.Monad
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
import qualified Exception                       (ExceptionMonad)
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

type ClientSend = ClientDirective -> IO ()

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

#if __GLASGOW_HASKELL__ >= 706
type ErrorMsg = (GHC.DynFlags,ErrUtils.MsgDoc)
#else
type ErrorMsg = ErrorUtils.Message
#endif

data ErrorInfo = ErrorMsg
  { _severity :: GHC.Severity
  , _location :: GHC.SrcSpan
  , _style    :: Outputable.PprStyle
  , _message  :: ErrorMsg
  }
makeLenses ''ErrorInfo

data State = State
    { stateWarningsEnabled :: Bool
    , currentFileReal      :: GHC.FastString
    , currentFilePath      :: GHC.FastString
    , cabalFileMod         :: EpochTime
    }

defaultOptions :: Options
defaultOptions = Options
  { _cabalMod = 0
  , _ghcOpts = []
  , _needReconfig = True
  }

newCommandLoopState :: IO (IORef State)
newCommandLoopState =
    newIORef State
        { stateWarningsEnabled = True
        , currentFileReal = GHC.fsLit ""
        , currentFilePath = GHC.fsLit ""
        , cabalFileMod = 0
        }

withWarnings :: (MonadIO m, Exception.ExceptionMonad m) => IORef State -> Bool -> m a -> m a
withWarnings state warningsValue action = do
    beforeState <- liftIO getWarnings
    liftIO $ setWarnings warningsValue
    action `GHC.gfinally` liftIO (setWarnings beforeState)
    where
    getWarnings :: IO Bool
    getWarnings = stateWarningsEnabled <$> readIORef state
    setWarnings :: Bool -> IO ()
    setWarnings val = modifyIORef state $ \s -> s { stateWarningsEnabled = val }

commandLoop :: (Proxy p) => Input ErrorInfo -> () -> Pipe (StateP Options (ReaderP Settings p)) CommandObj ClientDirective IO ()
commandLoop errorIn () = do
  cabalOpts <- lift cabalMiscOptions
  settings <- liftP ask
  void $ hoist (liftIO . GHC.runGhc (Just GHC.Paths.libdir)) $ do
    forever $ do
      opts <- use ghcOpts
      configOk <- lift $ GHC.gcatch (newSession settings errorIn (cabalOpts ++ opts) >> return True) handleConfigError
      unless configOk $ needReconfig .= True

  return ()

processError :: () -> Pipe p ErrorInfo ClientDirective IO ()
processError = error "todo"

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
    runProxy $ recvS errorOut >-> processError >-> sendD serverInput
    performGC

  return (clientInput,serverOutput)


  -- hoist (liftIO . GHC.runGhc (Just GHC.Paths.libdir)) $ runStateP defaultOptions $ do
  --   forever $ do
  --     configOk <- lift $ GHC.gcatch (newSession (cabalOpts ++ initialGhcOpts) >> return True) handleConfigError
  --     command <- request ()
  --     return ()

newSession :: Settings -> Input ErrorInfo -> [String] -> GHC.Ghc ()
newSession settings errorIn opts = do
  initialDynFlags <- GHC.getSessionDynFlags
  let updatedDynFlags = initialDynFlags
       { GHC.log_action = logAction' errorIn settings
       , GHC.ghcLink = GHC.NoLink
       , GHC.hscTarget = GHC.HscInterpreted
       }
  (finalDynFlags, _, _) <- GHC.parseDynamicFlags updatedDynFlags (map GHC.noLoc opts)
  void $ GHC.setSessionDynFlags finalDynFlags


handleConfigError :: GHC.GhcException -> GHC.Ghc Bool
handleConfigError = error "todo"

startCommandLoop :: Maybe FilePath -> IORef State -> ClientSend -> IO (Maybe CommandObj) -> [String] -> Maybe Command -> IO ()
startCommandLoop cabal state clientSend getNextCommand initialGhcOpts mbInitial = do
    cabalOpts <- cabalMiscOptions
    continue <- GHC.runGhc (Just GHC.Paths.libdir) $ do
        configOk <- GHC.gcatch (configSession state clientSend (cabalOpts ++ initialGhcOpts) >> return True)
            handleConfigError
        if configOk
            then do
                doMaybe mbInitial $ \cmd -> sendErrors (runCommand cabal state clientSend cmd)
                processNextCommand False
            else processNextCommand True

    case continue of
        Nothing ->
            -- Exit
            return ()
        Just (cmd, ghcOpts) -> startCommandLoop cabal state clientSend getNextCommand ghcOpts (Just cmd)
    where
    processNextCommand :: Bool -> GHC.Ghc (Maybe CommandObj)
    processNextCommand forceReconfig = do
        mbNextCmd <- liftIO getNextCommand
        case mbNextCmd of
            Nothing ->
                -- Exit
                return Nothing
            Just (cmd, ghcOpts) ->
                if forceReconfig || (ghcOpts /= initialGhcOpts)
                    then return (Just (cmd, ghcOpts))
                    else sendErrors (runCommand cabal state clientSend cmd) >> processNextCommand False

    sendErrors :: GHC.Ghc () -> GHC.Ghc ()
    sendErrors action = GHC.gcatch action $ void . handleConfigError

    handleConfigError :: GHC.GhcException -> GHC.Ghc Bool
    handleConfigError e = do
        liftIO $ mapM_ clientSend
            [ ClientStderr (GHC.showGhcException e "")
            , ClientExit (ExitFailure 1)
            ]
        return False

doMaybe :: Monad m => Maybe a -> (a -> m ()) -> m ()
doMaybe Nothing _ = return ()
doMaybe (Just x) f = f x

configSession :: IORef State -> ClientSend -> [String] -> GHC.Ghc ()
configSession state clientSend ghcOpts = do
    initialDynFlags <- GHC.getSessionDynFlags
    let updatedDynFlags = initialDynFlags
            { GHC.log_action = logAction state clientSend
            , GHC.ghcLink = GHC.NoLink
            , GHC.hscTarget = GHC.HscInterpreted
            }
    (finalDynFlags, _, _) <- GHC.parseDynamicFlags updatedDynFlags (map GHC.noLoc ghcOpts)
    _ <- GHC.setSessionDynFlags finalDynFlags
    return ()


cabalCached :: (GHC.MonadIO m, Functor m) => IORef State -> FilePath -> m () -> m ()
cabalCached state file act = do
  s <- GHC.liftIO $ getSymbolicLinkStatus file
  told <- fmap cabalFileMod $ GHC.liftIO $ readIORef state
  when (told < modificationTime s) $ do
    GHC.liftIO $ modifyIORef state $ \x -> x { cabalFileMod = (modificationTime s)}
    act

setCabalPerFileOpts :: ClientSend -> IORef State -> FilePath -> FilePath -> GHC.Ghc ()
setCabalPerFileOpts send state file configPath = cabalCached state configPath $ do
  GHC.liftIO $ send $ ClientLog $ "Loading cabal"
  f <- GHC.liftIO $ makeRelativeToCurrentDirectory file
  config <- GHC.liftIO $ readFile configPath
  case parseCabalConfig config of
    (ParseFailed _) -> GHC.liftIO $ send $ ClientStderr "Parsing of the cabal config failed. Please correct it, or use --no-cabal."
    (ParseOk _ r) -> do
      let srcInclude = "-i" ++ takeDirectory file
      let opts = fromMaybe [] $ getBuildInfoOptions =<< findBuildInfoFile r f
      dynFlags <- GHC.getSessionDynFlags
      (finalDynFlags, _, _) <- GHC.parseDynamicFlags dynFlags (map GHC.noLoc $ srcInclude : opts)
      _ <- GHC.setSessionDynFlags finalDynFlags
      return ()

setAllCabalImportDirs :: FilePath -> GHC.Ghc ()
setAllCabalImportDirs cabal = do
  config <- GHC.liftIO $ readFile cabal
  case parseCabalConfig config of
    (ParseFailed _) -> return ()
    (ParseOk _ r) -> do
      let dirs = allBuildInfo r >>= hsSourceDirs
      dynFlags <- GHC.getSessionDynFlags
      (finalDynFlags,_,_) <- GHC.parseDynamicFlags dynFlags (map (GHC.noLoc . ("-i" ++)) dirs)
      _ <- GHC.setSessionDynFlags finalDynFlags
      return ()

setCurrentFile :: IORef State -> FilePath -> FilePath -> GHC.Ghc ()
setCurrentFile state real path =
    liftIO $ modifyIORef state $ \x -> x
      { currentFileReal = GHC.fsLit real
      , currentFilePath = GHC.fsLit path
      }

runCommand :: Maybe FilePath -> IORef State -> ClientSend -> Command -> GHC.Ghc ()
runCommand cabal state clientSend (CmdCheck real file) = do
    let log = GHC.liftIO . clientSend . ClientLog
    log $ "[Check] Initalize state"
    setCurrentFile state real file
    log $ "[Check] Load cabal"
    let noPhase = Nothing
    doMaybe cabal $ setCabalPerFileOpts clientSend state file
    log $ "[Check] Load target"
    target <- GHC.guessTarget file noPhase
    GHC.setTargets [target]
    let handler err = GHC.printException err >> return GHC.Failed
    log $ "[Check] Compile target"
    flag <- GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
    log $ "[Check] Send result"
    liftIO $ case flag of
        GHC.Succeeded -> clientSend (ClientExit ExitSuccess)
        GHC.Failed -> clientSend (ClientExit (ExitFailure 1))
runCommand cabal _ clientSend (CmdModuleFile moduleName) = do
    doMaybe cabal setAllCabalImportDirs
    target <- GHC.guessTarget moduleName Nothing
    GHC.setTargets [target]
    -- TODO: This currently fails, need to figure out why (when cabal support enabled)
    moduleGraph <- GHC.depanal [] False `GHC.gcatch` \(SomeException _) -> return []
    case find (moduleSummaryMatchesModuleName moduleName) moduleGraph of
        Nothing ->
            liftIO $ mapM_ clientSend
                [ ClientStderr "Module not found"
                , ClientExit (ExitFailure 1)
                ]
        Just modSummary ->
            case GHC.ml_hs_file (GHC.ms_location modSummary) of
                Nothing ->
                    liftIO $ mapM_ clientSend
                        [ ClientStderr "Module does not have a source file"
                        , ClientExit (ExitFailure 1)
                        ]
                Just file ->
                    liftIO $ mapM_ clientSend
                        [ ClientStdout file
                        , ClientExit ExitSuccess
                        ]
    where
    moduleSummaryMatchesModuleName modName modSummary =
        modName == (GHC.moduleNameString . GHC.moduleName . GHC.ms_mod) modSummary
runCommand cabal state clientSend (CmdInfo real file identifier) = do
    setCurrentFile state real file
    doMaybe cabal $ setCabalPerFileOpts clientSend state file
    result <- withWarnings state False $
        getIdentifierInfo file identifier
    case result of
        Left err ->
            liftIO $ mapM_ clientSend
                [ ClientStderr err
                , ClientExit (ExitFailure 1)
                ]
        Right info -> liftIO $ mapM_ clientSend
            [ ClientStdout info
            , ClientExit ExitSuccess
            ]
runCommand cabal state clientSend (CmdType real file (line, col)) = do
    setCurrentFile state real file
    doMaybe cabal $ setCabalPerFileOpts clientSend state file
    result <- withWarnings state False $
        getType file (line, col)
    case result of
        Left err ->
            liftIO $ mapM_ clientSend
                [ ClientStderr err
                , ClientExit (ExitFailure 1)
                ]
        Right types -> liftIO $ do
            mapM_ (clientSend . ClientStdout . formatType) types
            clientSend (ClientExit ExitSuccess)
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

modifySrcSpan :: GHC.FastString -> GHC.FastString -> GHC.SrcSpan -> GHC.SrcSpan
modifySrcSpan _ _ srcspan@(GHC.UnhelpfulSpan _) = srcspan
modifySrcSpan real path (GHC.RealSrcSpan srcspan)
  | GHC.srcSpanFile srcspan == path = GHC.RealSrcSpan $ GHC.mkRealSrcSpan (GHC.mkRealSrcLoc real lineStart colStart) (GHC.mkRealSrcLoc real lineEnd colEnd)
  | otherwise = GHC.RealSrcSpan $ GHC.mkRealSrcSpan (GHC.mkRealSrcLoc real 1 1) (GHC.mkRealSrcLoc real 1 1)
  where colStart = GHC.srcSpanStartCol srcspan
        colEnd = GHC.srcSpanEndCol srcspan
        lineStart = GHC.srcSpanStartLine srcspan
        lineEnd = GHC.srcSpanEndLine srcspan

#if __GLASGOW_HASKELL__ >= 706


logAction :: IORef State -> ClientSend -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction state clientSend dflags severity srcspan style msg = do
    s <- readIORef state
    let out = Outputable.renderWithStyle dflags fullMsg style
        _ = severity
        fullMsg = ErrUtils.mkLocMessage severity (modifySrcSpan (currentFileReal s) (currentFilePath s) srcspan) msg'
        msg'
          | maybe False (== currentFilePath s) (GHC.srcSpanFileName_maybe srcspan) = msg
          | otherwise = maybe (Outputable.text "") Outputable.ftext (GHC.srcSpanFileName_maybe srcspan) Outputable.<> Outputable.text ":" Outputable.<+> msg
    logActionSend s clientSend severity out
#else
logAction :: FilePath -> FilePath -> IORef State -> ClientSend -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO ()
logAction state clientSend severity srcspan style msg = do
    s <- readIORef state
    let out = Outputable.renderWithStyle fullMsg style
        _ = severity
        fullMsg = ErrUtils.mkLocMessage (modifySrcSpan (currentFileReal s) (currentFilePath s) srcspan) msg
    logActionSend s clientSend severity out

#endif

logActionSend :: State -> ClientSend -> GHC.Severity -> String -> IO ()
logActionSend currentState clientSend severity out =
    when (not (isWarning severity) || stateWarningsEnabled currentState) $
        clientSend (ClientStdout out)
    where
    isWarning :: GHC.Severity -> Bool
    isWarning GHC.SevWarning = True
    isWarning _ = False

logAction' :: Input ErrorInfo -> Settings -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction' errorIn s dflags severity sspan style doc = void $ atomically $ send errorIn msg
  where msg = error "todo"
