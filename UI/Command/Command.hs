module UI.Command.Command (
	Command (..),
        defCmd
) where

import Data.Default

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans (liftIO)

import UI.Command.App (AppContext(..))

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
-- > worldHandler = liftIO $ putStrLn "Hello world!"
--
data (Default config) => Command config = Command {
    -- | Name of the cmdcommand
    cmdName :: String,

    -- | Handler
    -- cmdHandler :: App config (),
    cmdHandler :: ReaderT (AppContext config) IO (),

    -- | Category in this program's documentation
    cmdCategory :: String,

    -- | Synopsis
    cmdSynopsis :: String,

    -- | Short description
    cmdShortDesc :: String,

    -- | [(example description, args)]
    cmdExamples :: [(String, String)]
}

instance (Default config) => Default (Command config) where
    def = Command "<Anonymous command>"
                     (liftIO $ putStrLn "Unimplemented command")
		     def def def def

defCmd :: Command ()
defCmd = Command "<Anonymous command>"
                 (liftIO $ putStrLn "Unimplmented command")
                 "" "" "" []
