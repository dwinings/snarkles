module StringUtils where

import Data.Char

isNum :: [Char] -> Bool
isNum []     = True
isNum (x:xs) | isDigit x = isNum xs
             | otherwise = False

dropIncluding :: Char -> String -> String
dropIncluding c s = drop 1 . dropWhile (\x -> not $ x == c) $ s

dropUntil :: Char -> String -> String
dropUntil c s = dropWhile (\x -> not $ x == c) $ s


takeWord :: String -> String
takeWord s = takeWhile (\x -> not $ x == ' ') s
