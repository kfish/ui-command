module UI.Command.Main (
	subMain
) where

import Control.Monad (when)

import System.Environment (getArgs)
import System.Exit

import UI.Command.Application
import UI.Command.Command
import UI.Command.Doc

------------------------------------------------------------
-- subMain
--

-- | Main wrapper
--
-- > main = subMain hello
--
subMain :: Application -> IO ()
subMain cmd = do
        allArgs <- getArgs
	when (any isHelp allArgs) $ showHelp cmd allArgs
	when (any isVersion allArgs) $ showVersion cmd
	handleSubCommand cmd allArgs

helpStrings :: [[Char]]
helpStrings = ["--help", "-h", "-?"]

versionStrings :: [[Char]]
versionStrings = ["--version", "-V"]

isHelp :: String -> Bool
isHelp x = elem x helpStrings

isVersion :: String -> Bool
isVersion x = elem x versionStrings

showHelp :: Application -> [String] -> IO ()
showHelp cmd args = do
        help cmd args
	exitWith ExitSuccess

showVersion :: Application -> IO ()
showVersion cmd = do
        putStrLn $ appName cmd ++ " " ++ appVersion cmd
        exitWith ExitSuccess

handleSubCommand :: Application -> [String] -> IO ()
handleSubCommand cmd [] = showHelp cmd []
-- handleSubCommand cmd [_] = showHelp cmd [""]

handleSubCommand cmd (command:rest)
        | command == "help" = help cmd rest
        | command == "man" = man cmd rest
        | otherwise = act ss
        where
	        ss = filter (\x -> subName x == command) (appSubs cmd)
	        act [] = help cmd [command]
		act (s:_) = (subHandler s) rest
