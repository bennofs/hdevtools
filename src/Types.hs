module Types
    ( ServerDirective(..)
    , ClientDirective(..)
    , Command(..)
    ) where

import           System.Exit (ExitCode)

data ServerDirective
    = SrvCommand Command [String]
    | SrvStatus
    | SrvExit
    deriving (Read, Show)

data ClientDirective
    = ClientStdout String
    | ClientStderr String
    | ClientLog String
    | ClientExit ExitCode
    | ClientUnexpectedError String -- ^ For unexpected errors that should not happen
    deriving (Read, Show)


-- | Commands taking a file take the "real" file path (as it was given on the commandline) and the absolute file path.
data Command
    = CmdCheck FilePath FilePath
    | CmdModuleFile String
    | CmdInfo FilePath FilePath String
    | CmdType FilePath FilePath (Int, Int)
    deriving (Read, Show)
