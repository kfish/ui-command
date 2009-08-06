module UI.Command.Application (
        Application (..),
) where

import Data.Default

import System.Console.GetOpt (OptDescr)

import UI.Command.Command

------------------------------------------------------------
-- Application class
--

-- | It is often simpler to use the default implementation of Application, and
-- override it with the details you choose to use.
-- For example, an implementation of the ''hello'' command:
--
-- > hello = def {
-- >         appName = "hello",
-- >         appVersion = "0.1",
-- >         appAuthors = ["Joe R. Hacker"],
-- >         appBugEmail = "bugs@example.com",
-- >         appShortDesc = "UI.Command example program",
-- >         appLongDesc = longDesc,
-- >         appCategories = ["Greetings", "Cat Math"],
-- >         appSeeAlso = ["tractorgen"],
-- >         appProject = "Haskell",
-- >         appCmds = [world, times]
-- > }
-- > 
-- > longDesc = "a demonstration program for the UI.Command framework."
--
data (Default opts, Default config) => Application opts config = Application {
    -- | Name of the program
    appName :: String,

    -- | Software version
    appVersion :: String,

    -- | Email address to report bugs to
    appBugEmail :: String,

    -- | Names of authors
    appAuthors :: [String],

    -- | One-line description of the command
    appShortDesc :: String,

    -- | Long description of the command
    appLongDesc :: String,

    -- | Categories to show in help text, in order of appearance
    appCategories :: [String],

    -- | Project that this command is part of
    appProject :: String,

    -- | Related commands
    appSeeAlso :: [String],

    -- | The actual commands
    appCmds :: [Command config],

    -- | Union of all options accepted by the application's commands.
    -- Note that options '-h', '-?', '--help', '-V', '--version' will
    -- be automatically added and handled by UI.Command
    appOptions :: [OptDescr opts],

    -- | Function to process options
    appProcessConfig :: config -> [opts] -> IO config
}

instance (Default opts, Default config) => Default (Application opts config) where
    def = Application "<undocumented command>" "0.0" def def def def def def def def def def

