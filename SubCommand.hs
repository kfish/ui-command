module SubCommand (
        Command (..),
	SubCommand (..),
	subMain
)where

import Control.Monad (when)

import Data.Char (isSpace)
import Data.List (intersperse)

import System.Environment (getArgs)
import System.Exit

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

------------------------------------------------------------
-- Command class
--

data (SubCommand a) => Command a = Command {
    commandName :: String,
    commandVersion :: String,

    -- Email address to report bugs to
    commandBugEmail :: String,

    commandDesc :: String,

    -- Categories to show in help text, in order of appearance
    commandCategories :: [String],

    -- The actual subcommands
    commandSubs :: [a]
}

------------------------------------------------------------
-- SubCommand class
--

class SubCommand a where
    subName :: a -> String

    subMethod :: a -> [String] -> IO ()
    subMethod _ _ = putStrLn "Unimplemented command"

    subCategory :: a -> String
    subSynopsis _ = ""

    subSynopsis :: a -> String
    subSynopsis _ = ""

    subDescription :: a -> String
    subDescription _ = ""

    -- [(example description, args)]
    subExamples :: a -> [(String, String)]
    subExamples _ = []

------------------------------------------------------------
-- Help
--

{-
helpSub :: SubCommand
helpSub = SubCommand "help" help
    "Commands" ("Display help for a specific subcommand (eg. \"" ++ progname ++ " help dump\")")
-}

help :: (SubCommand a) => Command a -> [String] -> IO ()
help cmd args = mapM_ putStr $ longHelp cmd args

longHelp :: (SubCommand a) => Command a -> [String] -> [String]
-- | "cmd help" with no arguments: Give a list of all subcommands
longHelp cmd [] =
    ["Usage: " ++ (commandName cmd) ++ " [--version] [--help] command [args]\n\n"] ++
    [indent 2 (commandDesc cmd), "\n"] ++
    map (categoryHelp cmd) (commandCategories cmd) ++
    ["Please report bugs to <" ++ commandBugEmail cmd ++ ">\n"]

-- | "cmd help command": Give command-specific help
longHelp cmd (command:_) = contextHelp cmd command m
  where m = filter (\x -> subName x == command) (commandSubs cmd)

-- | Provide synopses for a specific category of commands
categoryHelp :: (SubCommand a) => Command a -> String -> String
categoryHelp cmd c = c ++ ":\n" ++ concat (map itemHelp items) ++ "\n"
     where
        items = filter (\x -> subCategory x == c) (commandSubs cmd)
        itemHelp i = printf "  %-14s%s\n" (subName i) (subSynopsis i)

-- | Provide detailed help for a specific command
contextHelp :: (SubCommand a) => Command a -> [Char] -> [a] -> [String]
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

subMain :: (SubCommand a) => Command a -> IO ()
subMain cmd = do
        allArgs <- getArgs
	when (any isHelp allArgs) $ showHelp cmd allArgs
	when (any isVersion allArgs) $ showVersion cmd
	handleSubCommand cmd allArgs

showHelp :: (SubCommand a) => Command a -> [String] -> IO ()
showHelp cmd args = do
        help cmd args
	exitWith ExitSuccess

showVersion :: (SubCommand a) => Command a -> IO ()
showVersion cmd = do
        putStrLn $ commandName cmd ++ " " ++ commandVersion cmd
        exitWith ExitSuccess

handleSubCommand :: (SubCommand a) => Command a -> [String] -> IO ()
handleSubCommand cmd [] = showHelp cmd [""]
-- handleSubCommand cmd [_] = showHelp cmd [""]

handleSubCommand cmd (command:rest)
        | command == "help" = help cmd rest
        | otherwise = act ss
        where
	        ss = filter (\x -> subName x == command) (commandSubs cmd)
	        act [] = help cmd []
		act (s:_) = (subMethod s) rest
