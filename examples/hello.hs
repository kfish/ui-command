module Main where

import Data.Default (def)
import Data.List (intersperse)

import UI.Command

------------------------------------------------------------
-- world
--

world = def {
       	        subName = "world",
                subHandler = worldHandler,
                subCategory = "Greetings",
                subShortDesc = "An implementation of the standard software greeting."
        }

worldHandler :: [String] -> IO ()
worldHandler _ = putStrLn "Hello world!"

------------------------------------------------------------
-- times
--

times = def {
       	        subName = "times",
                subHandler = timesHandler,
                subCategory = "Cat Math",
                subShortDesc = "A repetition of salutation",
                subExamples = [("Say hello 7 times", "7"), ("Say hello 3 times", "3")]
        }

timesHandler :: [String] -> IO ()
timesHandler [] = return ()
timesHandler (n:_) = putStrLn $ concat . intersperse " " $ take (read n) (repeat "hello")

------------------------------------------------------------
-- The command
--

hello = def {
	        commandName = "hello",
                commandVersion = "0.1",
		commandAuthors = ["Joe R. Hacker"],
                commandBugEmail = "bugs@example.com",
                commandShortDesc = "Subcommand example program",
                commandLongDesc = longDesc,
	        commandCategories = ["Greetings", "Cat Math"],
		commandSeeAlso = ["tractorgen"],
		commandProject = "Haskell",
	        commandSubs = [world, times]
	}

longDesc = "a demonstration program for the UI.Command framework."

------------------------------------------------------------
-- Main
--

main = subMain hello
