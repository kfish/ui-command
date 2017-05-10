module UI.Command.App (
        App,
        appArgs,
        appConfig,

        AppContext(..)
) where

import Data.Default

import Control.Monad.Reader
import System.Console.GetOpt

import Control.Monad (when)

import System.Environment (getArgs)
import System.Exit

------------------------------------------------------------
-- App
--

data (Default config) => AppContext config = AppContext {
        appContextConfig :: config,
        appContextArgs :: [String]
}

type App config = ReaderT (AppContext config) IO

-- | Get the application arguments
appArgs :: (Default config) => App config [String]
appArgs = asks appContextArgs

-- | Get the application config
appConfig :: (Default config) => App config config
appConfig = asks appContextConfig
