module Main where

import Control.Monad (liftM, when)
import Control.Monad.Trans (liftIO)

import Data.Default
import Data.List (intersperse)

import UI.Command

------------------------------------------------------------
-- world
--

world :: Command ()

world = defCmd {
       	        cmdName = "world",
                cmdHandler = worldHandler,
                cmdCategory = "Greetings",
                cmdShortDesc = "An implementation of the standard software greeting."
        }

worldHandler = liftIO $ putStrLn "Hello world!"

------------------------------------------------------------
-- times
--

times = defCmd {
       	        cmdName = "times",
                cmdHandler = timesHandler,
                cmdCategory = "Cat Math",
                cmdShortDesc = "A repetition of salutation",
                cmdExamples = [("Say hello 7 times", "7"), ("Say hello 3 times", "3")]
        }

timesHandler = do
        args <- appArgs
        when (args == []) $ return ()
        liftIO $ putStrLn $ concat . intersperse " " $ take (read $ head args) (repeat "hello")

------------------------------------------------------------
-- The Application
--

hello :: Application () ()
hello = def {
	        appName = "hello",
                appVersion = "0.1",
		appAuthors = ["Joe R. Hacker"],
                appBugEmail = "bugs@example.com",
                appShortDesc = "UI.Command example program",
                appLongDesc = longDesc,
	        appCategories = ["Greetings", "Cat Math"],
		appSeeAlso = ["tractorgen"],
		appProject = "Haskell",
	        appCmds = [world, times]
	}

longDesc = "a demonstration program for the UI.Command framework."

------------------------------------------------------------
-- Main
--

main :: IO ()
main = appMain hello
