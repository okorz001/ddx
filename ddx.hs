module Ddx where

import Data.Char

tokenize str = tokenize' str []

-- Identify the start of new tokens. If it's a simple token (i.e. single
-- character) we can add it directly to the token list.
tokenize' [] toks = toks
tokenize' (x:xs) toks
    | isSpace x  = tokenize' xs toks
    | isLetter x = tokenizeName xs toks [x]
    | isDigit x  = tokenizeNum xs toks [x]
    | otherwise  = tokenize' xs $ toks ++ [[x]]

tokenizeName [] toks word = toks ++ [word]
tokenizeName (x:xs) toks word
    | isLetter x = tokenizeName xs toks $ word ++ [x]
    | otherwise  = tokenize' (x:xs) $ toks ++ [word]

tokenizeNum [] toks word = toks ++ [word]
tokenizeNum (x:xs) toks word
    | isDigit x = tokenizeNum xs toks $ word ++ [x]
    | otherwise = tokenize' (x:xs) $ toks ++ [word]
