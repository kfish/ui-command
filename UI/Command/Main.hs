module UI.Command.Main (
	appMain
) where

import Control.Monad (when)

import System.Environment (getArgs)
import System.Exit

import UI.Command.Application
import UI.Command.Command
import UI.Command.Doc

------------------------------------------------------------
-- appMain
--

-- | Main wrapper
--
-- > main = appMain hello
--
appMain :: Application -> IO ()
appMain app = do
        allArgs <- getArgs
	when (any isHelp allArgs) $ showHelp app allArgs
	when (any isVersion allArgs) $ showVersion app
	handleCommand app allArgs

helpStrings :: [[Char]]
helpStrings = ["--help", "-h", "-?"]

versionStrings :: [[Char]]
versionStrings = ["--version", "-V"]

isHelp :: String -> Bool
isHelp x = elem x helpStrings

isVersion :: String -> Bool
isVersion x = elem x versionStrings

showHelp :: Application -> [String] -> IO ()
showHelp app args = do
        help app args
	exitWith ExitSuccess

showVersion :: Application -> IO ()
showVersion app = do
        putStrLn $ appName app ++ " " ++ appVersion app
        exitWith ExitSuccess

handleCommand :: Application -> [String] -> IO ()
handleCommand app [] = showHelp app []
-- handleCommand app [_] = showHelp app [""]

handleCommand app (command:rest)
        | command == "help" = help app rest
        | command == "man" = man app rest
        | otherwise = act ss
        where
	        ss = filter (\x -> cmdName x == command) (appCmds app)
	        act [] = help app [command]
		act (s:_) = (cmdHandler s) rest
