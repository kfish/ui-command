module UI.Command.Application (
        Application (..),
) where

import Data.Default

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
data Application = Application {
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
    appCmds :: [Command]
}

instance Default Application where
    def = Application "<undocumented command>" "0.0" def def def def def def def def

