module UI.Command.Doc (
	helpCmd, manCmd,
        helpErr, help, man
)where

import Data.Default
import Data.Char (toUpper)

import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)
import System.IO (hPutStr, stderr)

import Text.Printf (printf)

import Control.Monad.Trans (liftIO)

import UI.Command.App (App, appArgs)
import UI.Command.Application
import UI.Command.Command
import UI.Command.Render

------------------------------------------------------------
-- internal cmdcommands
--

internalCmds :: [Command ()]
internalCmds = [helpCmd, manCmd]

------------------------------------------------------------
-- Help
--

-- helpCmd :: (Default config) => Command config
helpCmd = def {
        cmdName = "help",
        cmdShortDesc = "Display help for a specific cmdcommand"
}

helpErr app = do
        args <- appArgs
	liftIO $ mapM_ (hPutStr stderr) $ longHelp app args

--help :: (Default opts, Default config) => Application opts config -> [String] -> IO ()
-- help :: (Default opts, Default config) => Application opts config -> App config ()
help app = do
        args <- appArgs
	liftIO $ mapM_ putStr $ longHelp app args

longHelp :: (Default opts, Default config) => Application opts config -> [String] -> [String]
-- | "app help" with no arguments: Give a list of all cmdcommands
longHelp app [] =
    [appShortDesc app ++ "\n"] ++
    ["Usage: " ++ (appName app) ++ " [--version] [--help] command [args]\n\n"] ++
    [indent 2 (appLongDesc app), "\n"] ++
    map (categoryHelp app) (appCategories app) ++
    [internalHelp app] ++
    ["\nPlease report bugs to <" ++ appBugEmail app ++ ">\n"]

-- | "app help command": Give command-specific help
longHelp app (command:_) = contextHelp app command m
  where m = filter (\x -> cmdName x == command) (appCmds app)

-- | Provide synopses for a specific category of commands
categoryHelp :: (Default opts, Default config) => Application opts config -> String -> String
categoryHelp app c = c ++ ":\n" ++ unlines (map itemHelp items) ++ "\n"
     where
        items = filter (\x -> cmdCategory x == c) (appCmds app)

-- | Provide synopses for internal commands
internalHelp :: (Default opts, Default config) => Application opts config -> String
internalHelp app = unlines $ "Miscellaneous:" : map itemHelp internalCmds

-- | One-line format for a command
itemHelp i = printf "  %-14s%s" (cmdName i) (cmdShortDesc i)

-- | Provide detailed help for a specific command
contextHelp :: (Default opts, Default config) => Application opts config -> [Char] -> [Command config] -> [String]
contextHelp app command [] = longHelp app [] ++ contextError
  where contextError = ["\n*** \"" ++ command ++ "\": Unknown command.\n"]
contextHelp app command (item:_) = synopsis ++ usage ++ description ++ examples
  where usage = ["Usage: " ++ appName app ++ " " ++ command ++ hasOpts command ++ "\n"]
        hasOpts "help" = " command"
        hasOpts _ = " [options]"
        synopsis = [(appName app) ++ " " ++ command ++ ": " ++ cmdSynopsis item ++ "\n"]
        description = case (cmdShortDesc item) of
                    "" -> []
                    _  -> ["\n" ++ indent 2 (cmdShortDesc item)]
        examples = case (cmdExamples item) of
                     [] -> []
                     _  -> ["\nExamples:"] ++
                           flip map (cmdExamples item) (\(desc,opts) ->
                             "\n  " ++ desc ++ ":\n    " ++ (appName app) ++ " " ++ command ++
                             " " ++ opts ++ "\n")

------------------------------------------------------------
-- man
--

manCmd :: (Default config) => Command config
manCmd = def {
        cmdName = "man",
        cmdShortDesc = "Generate Unix man page for specific cmdcommand"
}

-- man :: (Default opts, Default config) => Application opts config -> [String] -> IO ()
man app = do
        args <- appArgs
        currentTime <- liftIO $ getCurrentTime
	let dateStamp = formatTime defaultTimeLocale "%B %Y" currentTime
	liftIO $ putStrLn . concat $ longMan app dateStamp args

manSH :: String -> String
manSH s = "\n.SH " ++ s ++ "\n\n"

headerMan :: (Default opts, Default config) => Application opts config -> String -> [String]
headerMan app dateStamp = [unwords [".TH", u, "1", quote dateStamp, quote (appName app), project, "\n"]]
    where
        u = map toUpper (appName app)
	project | appProject app == def = ""
	        | otherwise = quote $ appProject app

synopsisMan :: (Default opts, Default config) => Application opts config -> String -> [Command config] -> [String]
synopsisMan app _ [] =
    [manSH "SYNOPSIS", ".B ", appName app, "\n.RI COMMAND\n[\n.I OPTIONS\n]\n.I filename ...\n\n"]
synopsisMan app command (item:_) =
    [manSH "SYNOPSIS", ".B ", appName app, "\n.RI ", command, "\n", hasOpts command, "\n"]
  where hasOpts "help" = ".I <cmdcommand>\n"
        hasOpts "man" = ".I <cmdcommand>\n"
        hasOpts _ = "[\n.I OPTIONS\n]\n"

authorsMan :: (Default opts, Default config) => Application opts config -> String -> [String]
authorsMan app command = manSH "AUTHORS" : a ++ g ++ e
  where
    n = appName app
    a | appAuthors app == [] = []
      | otherwise = [n ++ " was written by ", englishList $ appAuthors app, "\n\n"]
    g = ["This manual page was autogenerated by\n.B " ++ n ++ " man" ++ space command ++ ".\n\n"]
    e | appBugEmail app == "" = []
      | otherwise = ["Please report bugs to <" ++ appBugEmail app ++ ">\n"]
    space "" = ""
    space c = ' ':c

descMan :: String -> [String]
descMan desc = [manSH "DESCRIPTION", desc, "\n"]

longMan :: (Default opts, Default config) => Application opts config -> String -> [String] -> [String]
longMan app dateStamp [] =
        headerMan app dateStamp ++
	[manSH "NAME"] ++
        [appName app, " \\- ", appShortDesc app, "\n\n"] ++
        synopsisMan app "COMMAND" [] ++
        descMan (".B " ++ appName app ++ "\n" ++ appLongDesc app) ++
        map (categoryMan app) (appCategories app) ++
	authorsMan app "" ++
        seeAlsoMan app

longMan app dateStamp (command:_) = contextMan app dateStamp command m
    where
        m = filter (\x -> cmdName x == command) (appCmds app)

-- | Provide a list of related commands
seeAlsoMan :: (Default opts, Default config) => Application opts config -> [String]
seeAlsoMan app
        | appSeeAlso app == def = []
        | otherwise = [manSH "SEE ALSO" ++ ".PP\n"] ++
                      map (\x -> "\\fB"++x++"\\fR(1)\n") (appSeeAlso app)

-- | Provide synopses for a specific category of commands
categoryMan :: (Default opts, Default config) => Application opts config -> String -> String
categoryMan app c = manSH (map toUpper c) ++ concat (map itemMan items) ++ "\n"
  where items = filter (\x -> cmdCategory x == c) (appCmds app)
        itemMan i = printf ".IP %s\n%s\n" (cmdName i) (cmdShortDesc i)

contextMan :: (Default opts, Default config) => Application opts config -> String -> [Char] -> [Command config] -> [String]
contextMan app dateStamp _ [] = longMan app dateStamp []
contextMan app dateStamp command i@(item:_) =
        headerMan app dateStamp ++
        synopsisMan app command i ++
        descMan (cmdSynopsis item) ++
        description ++
        examples ++
	authorsMan app command
    where
        description | cmdShortDesc item == "" = []
                    | otherwise = ["\n" ++ cmdShortDesc item]
        examples | cmdExamples item == [] = []
                 | otherwise = manSH "EXAMPLES" :
                               flip map (cmdExamples item) (\(desc, opts) ->
                                 ".PP\n" ++ desc ++ ":\n.PP\n.RS\n\\f(CW" ++
                                 appName app ++ " " ++ command ++ " " ++
                                 opts ++ "\\fP\n.RE\n")
