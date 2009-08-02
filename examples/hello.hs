module Main where

import Data.List (intersperse)

import UI.SubCommand

data HelloSub = HelloSub {
        helloName :: String,
	helloMethod :: [String] -> IO (),
	helloCategory :: String,
	helloSynopsis :: String
}

instance SubCommand HelloSub where
        subName = helloName
	subMethod = helloMethod
	subCategory = helloCategory
	subSynopsis = helloSynopsis

------------------------------------------------------------
-- world
--

world = HelloSub "world" worldMethod "Greetings"
        "An implementation of the standard software greeting."

worldMethod :: [String] -> IO ()
worldMethod _ = putStrLn "Hello world!"

------------------------------------------------------------
-- times
--

times = HelloSub "times" timesMethod "Cat Math"
        "A repetition of salutation"

timesMethod :: [String] -> IO ()
timesMethod [] = return ()
timesMethod (n:_) = putStrLn $ concat . intersperse " " $ take (read n) (repeat "hello")

------------------------------------------------------------
-- The command
--

hello :: Command HelloSub
hello = Command "hello" "0.1"
        "bugs@example.com"
        "Subcommand example program"
	["Greetings", "Cat Math"]
	[world, times]

------------------------------------------------------------
-- Main
--

main = subMain hello
