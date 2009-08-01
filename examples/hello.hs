module Main where

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
-- The command
--

hello :: Command HelloSub
hello = Command "hello" "0.1"
        "bugs@example.com"
        "Subcommand example program"
	["Greetings"]
	[world]

------------------------------------------------------------
-- Main
--

main = subMain hello
