module UI.Command.Command (
        Command (..),
	SubCommand (..),
) where

import Data.Default

------------------------------------------------------------
-- Command class
--

data Command = Command {
    commandName :: String,
    commandVersion :: String,

    -- Email address to report bugs to
    commandBugEmail :: String,

    -- Names of authors
    commandAuthors :: [String],

    -- One-line description of the command
    commandShortDesc :: String,

    -- Long description of the command
    commandLongDesc :: String,

    -- Categories to show in help text, in order of appearance
    commandCategories :: [String],

    -- Related commands
    commandSeeAlso :: [String],

    -- The actual subcommands
    commandSubs :: [SubCommand]
}

instance Default Command where
    def = Command "<undocumented command>" "0.0" def def def def def def def

------------------------------------------------------------
-- SubCommand class
--

data SubCommand  = SubCommand {
    subName :: String,
    subHandler :: [String] -> IO (),
    subCategory :: String,
    subSynopsis :: String,
    subShortDesc :: String,

    -- [(example description, args)]
    subExamples :: [(String, String)]
}

instance Default SubCommand where
    def = SubCommand "<undocumented subcommand>"
                     (\_ -> putStrLn "Unimplemented command")
		     def def def def
