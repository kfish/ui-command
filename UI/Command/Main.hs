module UI.Command.Main (
	appMain,
        appMainWithOptions
) where

import Data.Default

import Control.Monad.Reader
import System.Console.GetOpt

import Control.Monad (when)

import System.Environment (getArgs)
import System.Exit

import UI.Command.App (AppContext(..))
import UI.Command.Application
import UI.Command.Command (Command, cmdName, cmdHandler)
import UI.Command.Doc

------------------------------------------------------------
--
--

initApp :: (Default opts, Default config)
        => Application opts config -> [String]
        -> IO (AppContext config)
initApp app args = do
        (config, args) <- processArgs app args
	return $ AppContext config args

processArgs :: (Default opts, Default config)
            => Application opts config -> [String]
            -> IO (config, [String])
processArgs app args = do
  case getOpt RequireOrder (appOptions app) args of
    (opts, args'  , []  ) -> do
                        config <- (appProcessConfig app) def opts
                        return (config, args')
    (_, _, _ : _) -> return (def, args)

------------------------------------------------------------
-- appMain
--

-- | Main wrapper
--
-- > main = appMain hello
--
appMain :: Application () () -> IO ()
appMain app = do
        allArgs <- getArgs
	when (any isHelp allArgs) $ showHelp app allArgs
	when (any isVersion allArgs) $ showVersion app
	handleCommand app allArgs

appMainWithOptions :: (Default opts, Default config) => Application opts config -> IO ()
appMainWithOptions app = do
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

showHelp :: (Default opts, Default config) => Application opts config -> [String] -> IO ()
showHelp app args = do
        (initApp app args) >>= runReaderT (help app)
	exitWith ExitSuccess

showVersion :: (Default opts, Default config) => Application opts config -> IO ()
showVersion app = do
        putStrLn $ appName app ++ " " ++ appVersion app
        exitWith ExitSuccess

handleCommand :: (Default opts, Default config) => Application opts config -> [String] -> IO ()
handleCommand app [] = showHelp app []
-- handleCommand app [_] = showHelp app [""]

handleCommand app (command:args)
        | command == "help" = showHelp app args
        | command == "man" = showMan
        | otherwise = initApp app args >>= loop1
        where
                showMan = initApp app args >>= loopMan
	        loopMan st = runReaderT (man app) st
	        loop1 st = runReaderT run st
                -- docCmds :: (Default config1) => [Command config1]
                -- docCmds = [helpCmd{cmdHandler = help app}, manCmd{cmdHandler = man app}]
                run = act $ filter (\x -> cmdName x == command) (appCmds app)
	        act [] = helpErr app
		act (s:_) = cmdHandler s
