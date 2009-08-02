module UI.Command.Render (
        para, indent, quote, breakLines, englishList
)where

import Data.Char (isSpace)
import Data.List (intersperse)

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
