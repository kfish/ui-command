module UI.Command.Command (
        Application (..),
	SubCommand (..),
) where

import Data.Default

------------------------------------------------------------
-- Application class
--

-- | It is often simpler to use the default implementation of Application, and
-- override it with the details you choose to use.
-- For example, an implementation of the "hello" command:
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
-- >         appSubs = [world, times]
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

    -- | The actual subcommands
    appSubs :: [SubCommand]
}

instance Default Application where
    def = Application "<undocumented command>" "0.0" def def def def def def def def

------------------------------------------------------------
-- SubCommand class
--

-- | It is often simpler to use the default implementation of SubCommand, and
-- override it with the details you choose to use.
-- For example, an implementation of the "hello world" command:
--
-- > world = def {
-- >         subName = "world",
-- >         subHandler = worldHandler,
-- >         subCategory = "Greetings",
-- >         subShortDesc = "An implementation of the standard software greeting."
-- > }
-- >
-- > worldHandler :: [String] -> IO ()
-- > worldHandler _ = putStrLn "Hello world!"
--
data SubCommand  = SubCommand {
    -- | Name of the subcommand
    subName :: String,

    -- | Handler
    subHandler :: [String] -> IO (),

    -- | Category in this program's documentation
    subCategory :: String,

    -- | Synopsis
    subSynopsis :: String,

    -- | Short description
    subShortDesc :: String,

    -- | [(example description, args)]
    subExamples :: [(String, String)]
}

instance Default SubCommand where
    def = SubCommand "<undocumented subcommand>"
                     (\_ -> putStrLn "Unimplemented command")
		     def def def def

