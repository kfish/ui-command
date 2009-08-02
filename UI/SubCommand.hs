module UI.SubCommand (
        Command (..),
	SubCommand (..),
	subMain
)where

import Control.Monad (when)

import Data.Default
import Data.Char (isSpace, toUpper)
import Data.List (intersperse)

import System.Environment (getArgs)
import System.Exit

import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)
import Data.Time.Clock (getCurrentTime)

import Text.Printf (printf)

------------------------------------------------------------
-- Paragraph rendering
--

para :: [String] -> String
para ss = concat $ intersperse "\n" (map (\s -> breakLines 76 s) ss)

indent :: Int -> String -> String
indent i s = unlines $ map (\x -> indentation ++ x) (lines s)
    where
        indentation = take i $ repeat ' '

quote :: String -> String
quote = surround "\""

surround :: [a] -> [a] -> [a]
surround c s = concat [c, s, c]

-- breakLines leftIndent columnWidth text
breakLines :: Int -> String -> String
breakLines n s
    | length s < n = s ++ "\n"
    | otherwise    = line' ++ "\n" ++ breakLines n rest'
    where
        (line, rest) = splitAt n s
        (rSpill, rLine) = break isSpace (reverse line)
        line' = reverse rLine
        rest' = reverse rSpill ++ rest

englishList :: [String] -> String
englishList [] = []
englishList [a] = a
englishList [a,b] = a ++ " and " ++ b
englishList (a:as) = a ++ ", " ++ englishList as

------------------------------------------------------------
-- Command class
--

data Command = Command {
    commandName :: String,
    commandVersion :: String,

    -- Email address to report bugs to
    commandBugEmail :: String,

    -- Names of authors
    commandAuthors :: [String],

    -- One-line description of the command
    commandShortDesc :: String,

    -- Long description of the command
    commandLongDesc :: String,

    -- Categories to show in help text, in order of appearance
    commandCategories :: [String],

    -- The actual subcommands
    commandSubs :: [SubCommand]
}

instance Default Command where
    def = Command "<undocumented command>" "0.0" def def def def def def

------------------------------------------------------------
-- SubCommand class
--

data SubCommand  = SubCommand {
    subName :: String,
    subMethod :: [String] -> IO (),
    subCategory :: String,
    subSynopsis :: String,
    subDescription :: String,

    -- [(example description, args)]
    subExamples :: [(String, String)]
}

instance Default SubCommand where
    def = SubCommand "<undocumented subcommand>"
                     (\_ -> putStrLn "Unimplemented command")
		     def def def def

------------------------------------------------------------
-- internal subcommands
--

internalSubs = [helpSub, manSub]

------------------------------------------------------------
-- Help
--

helpSub :: SubCommand
helpSub = def {subName = "help", subSynopsis="Display help for a specific subcommand"}

help :: Command -> [String] -> IO ()
help cmd args = mapM_ putStr $ longHelp cmd args

longHelp :: Command -> [String] -> [String]
-- | "cmd help" with no arguments: Give a list of all subcommands
longHelp cmd [] =
    ["Usage: " ++ (commandName cmd) ++ " [--version] [--help] command [args]\n\n"] ++
    [indent 2 (commandLongDesc cmd), "\n"] ++
    map (categoryHelp cmd) (commandCategories cmd) ++
    [internalHelp cmd] ++
    ["Please report bugs to <" ++ commandBugEmail cmd ++ ">\n"]

-- | "cmd help command": Give command-specific help
longHelp cmd (command:_) = contextHelp cmd command m
  where m = filter (\x -> subName x == command) (commandSubs cmd)

-- | Provide synopses for a specific category of commands
categoryHelp :: Command -> String -> String
categoryHelp cmd c = c ++ ":\n" ++ concat (map itemHelp items) ++ "\n"
     where
        items = filter (\x -> subCategory x == c) (commandSubs cmd)

-- | Provide synopses for internal commands
internalHelp :: Command -> String
internalHelp cmd = unlines $ "Miscellaneous:" : map itemHelp internalSubs

-- | One-line format for a command and its synopsis
itemHelp i = printf "  %-14s%s\n" (subName i) (subSynopsis i)

-- | Provide detailed help for a specific command
contextHelp :: Command -> [Char] -> [SubCommand] -> [String]
contextHelp cmd command [] = longHelp cmd [] ++ contextError
  where contextError = ["\n*** \"" ++ command ++ "\": Unknown command.\n"]
contextHelp cmd command (item:_) = synopsis ++ usage ++ description ++ examples
  where usage = ["Usage: " ++ commandName cmd ++ " " ++ command ++ hasOpts command ++ "\n"]
        hasOpts "help" = " command"
        hasOpts _ = " [options]"
        synopsis = [(commandName cmd) ++ " " ++ command ++ ": " ++ subSynopsis item ++ "\n"]
        description = case (subDescription item) of
                    "" -> []
                    _  -> ["\n" ++ indent 2 (subDescription item)]
        examples = case (subExamples item) of
                     [] -> []
                     _  -> ["\nExamples:"] ++
                           flip map (subExamples item) (\(desc,opts) ->
                             "\n  " ++ desc ++ ":\n    " ++ (commandName cmd) ++ " " ++ command ++
                             " " ++ opts ++ "\n")

------------------------------------------------------------
-- man
--

manSub :: SubCommand
manSub = def {subName="man", subSynopsis="Generate Unix man page for specific subcommand"}

man :: Command -> [String] -> IO ()
man cmd args = do
        currentTime <- getCurrentTime
	let dateStamp = formatTime defaultTimeLocale "%B %Y" currentTime
	mapM_ putStrLn $ longMan cmd dateStamp args

headerMan :: Command -> String -> [String]
headerMan cmd dateStamp = [unwords [".TH", u, "1", quote dateStamp, quote "Flim!", "\n"]]
    where
        u = map toUpper (commandName cmd)

synopsisMan :: Command -> String -> [SubCommand] -> [String]
synopsisMan cmd _ [] =
    [".SH SYNOPSIS\n\n.B ", commandName cmd, "\n.RI SUBCOMMAND\n[\n.I OPTIONS\n]\n.I filename ...\n\n"]
synopsisMan cmd command (item:_) =
    [".SH SYNOPSIS\n\n.B ", commandName cmd, "\n.RI ", command, "\n", hasOpts command, "\n"]
  where hasOpts "help" = ".I <subcommand>\n"
        hasOpts "man" = ".I <subcommand>\n"
        hasOpts _ = "[\n.I OPTIONS\n]\n"

authorsMan :: Command -> String -> [String]
authorsMan cmd command = a ++ g ++ e
  where
    n = commandName cmd
    a | commandAuthors cmd == [] = []
      | otherwise = [".SH AUTHORS\n\n" ++ n ++ " was written by ", englishList $ commandAuthors cmd, "\n\n"]
    g = ["This manual page was autogenerated by\n.B " ++ n ++ " man" ++ space command ++ ".\n\n"]
    e | commandBugEmail cmd == "" = []
      | otherwise = ["Please report bugs to <" ++ commandBugEmail cmd ++ ">\n"]
    space "" = ""
    space c = ' ':c

descMan :: String -> [String]
descMan desc = [".SH DESCRIPTION\n", desc, "\n"]

longMan :: Command -> String -> [String] -> [String]
longMan cmd dateStamp [] =
        headerMan cmd dateStamp ++
        [".SH NAME"] ++
        [commandName cmd, " \\- ", commandShortDesc cmd, "\n\n"] ++
        synopsisMan cmd "SUBCOMMAND" [] ++
        descMan (".B " ++ commandName cmd ++ "\n" ++ commandLongDesc cmd) ++
	authorsMan cmd ""

longMan cmd dateStamp (command:_) = contextMan cmd dateStamp command m
    where
        m = filter (\x -> subName x == command) (commandSubs cmd)

contextMan :: Command -> String -> [Char] -> [SubCommand] -> [String]
contextMan cmd dateStamp _ [] = longMan cmd dateStamp []
contextMan cmd dateStamp command i@(item:_) =
        headerMan cmd dateStamp ++
        synopsisMan cmd command i ++
        descMan (subSynopsis item) ++
        description ++
        examples ++
	authorsMan cmd command
    where
        description | subDescription item == "" = []
                    | otherwise = ["\n" ++ subDescription item]
        examples | subExamples item == [] = []
                 | otherwise = ["\n.SH ExAMPLES\n"] ++
                               flip map (subExamples item) (\(desc, opts) ->
                                 ".PP\n" ++ desc ++ ":\n.PP\n.RS\n\\f(CW" ++
                                 commandName cmd ++ " " ++ command ++ " " ++
                                 opts ++ "\\fP\n.RE\n")

------------------------------------------------------------
-- subMain
--

helpStrings :: [[Char]]
helpStrings = ["--help", "-h", "-?"]

versionStrings :: [[Char]]
versionStrings = ["--version", "-V"]

isHelp :: String -> Bool
isHelp x = elem x helpStrings

isVersion :: String -> Bool
isVersion x = elem x versionStrings

subMain :: Command -> IO ()
subMain cmd = do
        allArgs <- getArgs
	when (any isHelp allArgs) $ showHelp cmd allArgs
	when (any isVersion allArgs) $ showVersion cmd
	handleSubCommand cmd allArgs

showHelp :: Command -> [String] -> IO ()
showHelp cmd args = do
        help cmd args
	exitWith ExitSuccess

showVersion :: Command -> IO ()
showVersion cmd = do
        putStrLn $ commandName cmd ++ " " ++ commandVersion cmd
        exitWith ExitSuccess

handleSubCommand :: Command -> [String] -> IO ()
handleSubCommand cmd [] = showHelp cmd []
-- handleSubCommand cmd [_] = showHelp cmd [""]

handleSubCommand cmd (command:rest)
        | command == "help" = help cmd rest
        | command == "man" = man cmd rest
        | otherwise = act ss
        where
	        ss = filter (\x -> subName x == command) (commandSubs cmd)
	        act [] = help cmd [command]
		act (s:_) = (subMethod s) rest
