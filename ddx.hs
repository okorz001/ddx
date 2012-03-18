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

data Expr = Var String | Const Double | Sum Expr Expr | Diff Expr Expr |
        Prod Expr Expr | Quo Expr Expr deriving(Show)

-- TODO: parse :: [Token] -> Expr

derive _ (Const _) = Const 0
-- Cannot use pattern matching to find equal parameters.
derive x (Var y) = if x == y then Const 1 else Var y
derive x (Sum a b) =
    let a' = derive x a
        b' = derive x b
    in  Sum a' b'
derive x (Diff a b) =
    let a' = derive x a
        b' = derive x b
    in  Diff a' b'
derive x (Prod a b) =
    let a' = derive x a
        b' = derive x b
    in  Sum (Prod a' b) (Prod a b')
derive x (Quo a b) =
    let a' = derive x a
        b' = derive x b
        top = Diff (Prod a' b) (Prod a b')
        bot = Prod b b
    in  Quo top bot
