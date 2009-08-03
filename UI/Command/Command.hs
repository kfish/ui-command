module UI.Command.Command (
	SubCommand (..),
) where

import Data.Default

------------------------------------------------------------
-- SubCommand class
--

-- | It is often simpler to use the default implementation of SubCommand, and
-- override it with the details you choose to use.
-- For example, an implementation of the ''hello world'' command:
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

