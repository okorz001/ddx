module Ddx where

import Data.Char

data Token = Name String | Value Double | Add | Sub | Mult | Div deriving(Show)

tokenize str = reverse $ tokenize' str []

-- Identify the start of new tokens. If it's a simple token (i.e. single
-- character) we can add it directly to the token list.
tokenize' [] toks = toks
tokenize' (x : xs) toks
    | isSpace x  = tokenize' xs toks
    | isLetter x = tokenizeName xs toks [x]
    | isDigit x  = tokenizeNum xs toks [x]
    | x == '+'   = tokenize' xs $ Add : toks
    | x == '-'   = tokenize' xs $ Sub : toks
    | x == '*'   = tokenize' xs $ Mult : toks
    | x == '/'   = tokenize' xs $ Div : toks
    | otherwise  = error $ "Unrecognized symbol: " ++ [x]

tokenizeName [] toks word = Name(reverse word) : toks
tokenizeName (x : xs) toks word
    | isLetter x = tokenizeName xs toks $ x : word
    | otherwise  = tokenize' (x : xs) $ Name(reverse word) : toks

tokenizeNum [] toks word = Value(read . reverse $ word) : toks
tokenizeNum (x : xs) toks word
    | isDigit x = tokenizeNum xs toks $ x : word
    | otherwise = tokenize' (x : xs) $ Value(read . reverse $ word) : toks
