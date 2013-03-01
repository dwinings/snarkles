module IRC.Parser where

import IRC
import StringUtils
import Data.Char
import Data.List

parseMessage' :: String -> ServerMessage
parseMessage' s | ":" `isPrefixOf` s = parseCmd (takeWord $ drop 1 s) $ dropIncluding ' ' s
                | otherwise          = parseCmd "" s

parseCmd :: String -> String -> ServerMessage
parseCmd prefix s | isDigit $ head s   = NUMERIC (read (take 3 s)) $ parseArgs [] s
                  | otherwise          = let body = parseArgs [] $ dropIncluding ' ' s in
                                           CMD prefix (takeWord s) (init body) (last body) 
                  

parseArgs :: [[Char]] -> String -> [String]
parseArgs accum s | s == ""            = reverse   accum
                  | ":" `isPrefixOf` s = reverse $ (drop 1 s):accum
                  | otherwise          = parseArgs  ((takeWord s):accum) $ dropIncluding ' ' s
