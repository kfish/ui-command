module UI.Command.Command (
        Command (..),
	SubCommand (..),
) where

import Data.Default

------------------------------------------------------------
-- Command class
--

-- | A Command
data Command = Command {
    -- | Name of the program
    commandName :: String,

    -- | Software version
    commandVersion :: String,

    -- | Email address to report bugs to
    commandBugEmail :: String,

    -- | Names of authors
    commandAuthors :: [String],

    -- | One-line description of the command
    commandShortDesc :: String,

    -- | Long description of the command
    commandLongDesc :: String,

    -- | Categories to show in help text, in order of appearance
    commandCategories :: [String],

    -- | Project that this command is part of
    commandProject :: String,

    -- | Related commands
    commandSeeAlso :: [String],

    -- | The actual subcommands
    commandSubs :: [SubCommand]
}

instance Default Command where
    def = Command "<undocumented command>" "0.0" def def def def def def def def

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

