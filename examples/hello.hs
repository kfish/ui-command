module Main where

import Data.Default (def)
import Data.List (intersperse)

import UI.Command

------------------------------------------------------------
-- world
--

world = def {
       	        cmdName = "world",
                cmdHandler = worldHandler,
                cmdCategory = "Greetings",
                cmdShortDesc = "An implementation of the standard software greeting."
        }

worldHandler :: [String] -> IO ()
worldHandler _ = putStrLn "Hello world!"

------------------------------------------------------------
-- times
--

times = def {
       	        cmdName = "times",
                cmdHandler = timesHandler,
                cmdCategory = "Cat Math",
                cmdShortDesc = "A repetition of salutation",
                cmdExamples = [("Say hello 7 times", "7"), ("Say hello 3 times", "3")]
        }

timesHandler :: [String] -> IO ()
timesHandler [] = return ()
timesHandler (n:_) = putStrLn $ concat . intersperse " " $ take (read n) (repeat "hello")

------------------------------------------------------------
-- The command
--

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

main = appMain hello
