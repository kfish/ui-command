module Main where

import Data.List (intersperse)

import SubCommand

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

world = HelloSub "world" worldMeth "Greetings"
        "An implementation of the standard software greeting."

worldMeth :: [String] -> IO ()
worldMeth _ = putStrLn "Hello world!"

------------------------------------------------------------
-- times
--

times = HelloSub "times" timesMeth "Cat Math"
        "A repetition of salutation"

timesMeth :: [String] -> IO ()
timesMeth [] = return ()
timesMeth (n:_) = putStrLn $ concat . intersperse " " $ take (read n) (repeat "hello")

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
