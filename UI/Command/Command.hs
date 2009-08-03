module UI.Command.Command (
	Command (..),
) where

import Data.Default

------------------------------------------------------------
-- Command class
--

-- | It is often simpler to use the default implementation of Command, and
-- override it with the details you choose to use.
-- For example, an implementation of the ''hello world'' command:
--
-- > world = def {
-- >         cmdName = "world",
-- >         cmdHandler = worldHandler,
-- >         cmdCategory = "Greetings",
-- >         cmdShortDesc = "An implementation of the standard software greeting."
-- > }
-- >
-- > worldHandler :: [String] -> IO ()
-- > worldHandler _ = putStrLn "Hello world!"
--
data Command  = Command {
    -- | Name of the cmdcommand
    cmdName :: String,

    -- | Handler
    cmdHandler :: [String] -> IO (),

    -- | Category in this program's documentation
    cmdCategory :: String,

    -- | Synopsis
    cmdSynopsis :: String,

    -- | Short description
    cmdShortDesc :: String,

    -- | [(example description, args)]
    cmdExamples :: [(String, String)]
}

instance Default Command where
    def = Command "<undocumented cmdcommand>"
                     (\_ -> putStrLn "Unimplemented command")
		     def def def def

