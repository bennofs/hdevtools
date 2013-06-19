{-# LANGUAGE CPP #-}
module CommandLoop
    ( newCommandLoopState
    , startCommandLoop
    ) where

import Control.Exception (SomeException(..))
import Control.Monad (when)
import Data.IORef
import Data.List (find)
import Data.Maybe
import Distribution.PackageDescription (hsSourceDirs, allBuildInfo)
import Distribution.ParseUtils
import MonadUtils (MonadIO, liftIO)
import System.Directory (makeRelativeToCurrentDirectory)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath (takeDirectory)
import qualified ErrUtils
import qualified Exception (ExceptionMonad)
import qualified GHC
import qualified GHC.Paths
import qualified MonadUtils as GHC
import qualified Outputable

import Cabal
import Types (ClientDirective(..), Command(..))
import Info (getIdentifierInfo, getType)

type CommandObj = (Command, [String])

type ClientSend = ClientDirective -> IO ()

data State = State
    { stateWarningsEnabled :: Bool
    }

newCommandLoopState :: IO (IORef State)
newCommandLoopState = do
    newIORef $ State
        { stateWarningsEnabled = True
        }

withWarnings :: (MonadIO m, Exception.ExceptionMonad m) => IORef State -> Bool -> m a -> m a
withWarnings state warningsValue action = do
    beforeState <- liftIO $ getWarnings
    liftIO $ setWarnings warningsValue
    action `GHC.gfinally`
        (liftIO $ setWarnings beforeState)
    where
    getWarnings :: IO Bool
    getWarnings = readIORef state >>= return . stateWarningsEnabled
    setWarnings :: Bool -> IO ()
    setWarnings val = modifyIORef state $ \s -> s { stateWarningsEnabled = val }

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
    sendErrors action = GHC.gcatch action (\x -> handleConfigError x >> return ())

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
  

setCabalPerFileOpts :: ClientSend -> FilePath -> FilePath -> GHC.Ghc ()
setCabalPerFileOpts send file configPath = do
  f <- GHC.liftIO $ makeRelativeToCurrentDirectory file
  config <- GHC.liftIO $ readFile configPath
  case parseCabalConfig config of 
    (ParseFailed _) -> GHC.liftIO $ send $ ClientStderr $ "Parsing of the cabal config failed. Please correct it, or use --no-cabal."
    (ParseOk _ r) -> do
      let srcInclude = "-i" ++ takeDirectory file
      let opts = fromMaybe [] $ getBuildInfoOptions =<< findBuildInfoFile r f
      dynFlags <- GHC.getSessionDynFlags
      (finalDynFlags, _, _) <- GHC.parseDynamicFlags dynFlags (map GHC.noLoc $ srcInclude : opts)
      _ <- GHC.setSessionDynFlags finalDynFlags
      return ()

setAllCabalImportDirs :: ClientSend -> FilePath -> GHC.Ghc ()
setAllCabalImportDirs send cabal = do
  config <- GHC.liftIO $ readFile cabal
  case parseCabalConfig config of
    (ParseFailed _) -> return ()
    (ParseOk _ r) -> do
      let dirs = allBuildInfo r >>= hsSourceDirs
      GHC.liftIO $ send $ ClientStdout $ show dirs
      dynFlags <- GHC.getSessionDynFlags
      (finalDynFlags,_,_) <- GHC.parseDynamicFlags dynFlags (map (GHC.noLoc . ("-i" ++)) dirs)
      _ <- GHC.setSessionDynFlags finalDynFlags
      return ()

runCommand :: Maybe FilePath -> IORef State -> ClientSend -> Command -> GHC.Ghc ()
runCommand cabal _ clientSend (CmdCheck file) = do
    let noPhase = Nothing
    doMaybe cabal $ setCabalPerFileOpts clientSend file
    target <- GHC.guessTarget file noPhase
    GHC.setTargets [target]
    let handler err = GHC.printException err >> return GHC.Failed
    flag <- GHC.handleSourceError handler (GHC.load GHC.LoadAllTargets)
    liftIO $ case flag of
        GHC.Succeeded -> clientSend (ClientExit ExitSuccess)
        GHC.Failed -> clientSend (ClientExit (ExitFailure 1))
runCommand cabal _ clientSend (CmdModuleFile moduleName) = do
    doMaybe cabal $ setAllCabalImportDirs clientSend
    target <- GHC.guessTarget moduleName Nothing
    GHC.setTargets [target]
    -- TODO: This currently fails, need to figure out why
    moduleGraph <- GHC.depanal [] False `GHC.gcatch` \(SomeException _) -> return []
    GHC.liftIO $ clientSend $ ClientStdout $ show $ map GHC.ms_location $ moduleGraph
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
runCommand cabal state clientSend (CmdInfo file identifier) = do
    doMaybe cabal $ setCabalPerFileOpts clientSend file
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
runCommand cabal state clientSend (CmdType file (line, col)) = do
    doMaybe cabal $ setCabalPerFileOpts clientSend file
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

#if __GLASGOW_HASKELL__ >= 706
logAction :: IORef State -> ClientSend -> GHC.DynFlags -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.MsgDoc -> IO ()
logAction state clientSend dflags severity srcspan style msg =
    let out = Outputable.renderWithStyle dflags fullMsg style
        _ = severity
    in logActionSend state clientSend severity out
    where fullMsg = ErrUtils.mkLocMessage severity srcspan msg
#else
logAction :: IORef State -> ClientSend -> GHC.Severity -> GHC.SrcSpan -> Outputable.PprStyle -> ErrUtils.Message -> IO ()
logAction state clientSend severity srcspan style msg =
    let out = Outputable.renderWithStyle fullMsg style
        _ = severity
    in logActionSend state clientSend severity out
    where fullMsg = ErrUtils.mkLocMessage srcspan msg
#endif

logActionSend :: IORef State -> ClientSend -> GHC.Severity -> String -> IO ()
logActionSend state clientSend severity out = do
    currentState <- readIORef state
    when (not (isWarning severity) || stateWarningsEnabled currentState) $
        clientSend (ClientStdout out)
    where
    isWarning :: GHC.Severity -> Bool
    isWarning GHC.SevWarning = True
    isWarning _ = False
