module Main where

import Data.Default (def)
import Data.List (intersperse)

import UI.SubCommand

------------------------------------------------------------
-- world
--

world = def {
       	        subName = "world",
                subMethod = worldMethod,
                subCategory = "Greetings",
                subSynopsis = "An implementation of the standard software greeting."
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
                subSynopsis = "A repetition of salutation"
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
                commandBugEmail = "bugs@example.com",
                commandDesc = "Subcommand example program",
	        commandCategories = ["Greetings", "Cat Math"],
	        commandSubs = [world, times]
	}

------------------------------------------------------------
-- Main
--

main = subMain hello
