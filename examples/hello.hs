module Main where

import Data.Default (def)
import Data.List (intersperse)

import UI.Command

------------------------------------------------------------
-- world
--

world = def {
       	        subName = "world",
                subMethod = worldMethod,
                subCategory = "Greetings",
                subShortDesc = "An implementation of the standard software greeting."
        }

worldMethod :: [String] -> IO ()
worldMethod _ = putStrLn "Hello world!"

------------------------------------------------------------
-- times
--

times = def {
       	        subName = "times",
                subMethod = timesMethod,
                subCategory = "Cat Math",
                subShortDesc = "A repetition of salutation",
                subExamples = [("Say hello 7 times", "7"), ("Say hello 3 times", "3")]
        }

timesMethod :: [String] -> IO ()
timesMethod [] = return ()
timesMethod (n:_) = putStrLn $ concat . intersperse " " $ take (read n) (repeat "hello")

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
	        commandSubs = [world, times]
	}

longDesc = "a demonstration program for the UI.Command framework."

------------------------------------------------------------
-- Main
--

main = subMain hello
