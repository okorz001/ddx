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
    | isDigit x  = tokenizeValue xs toks [x]
    | x == '+'   = tokenize' xs $ Add : toks
    | x == '-'   = tokenize' xs $ Sub : toks
    | x == '*'   = tokenize' xs $ Mult : toks
    | x == '/'   = tokenize' xs $ Div : toks
    | otherwise  = error $ "Unrecognized symbol: " ++ [x]

tokenizeName [] toks word = Name(reverse word) : toks
tokenizeName (x : xs) toks word
    | isLetter x = tokenizeName xs toks $ x : word
    | otherwise  = tokenize' (x : xs) $ Name(reverse word) : toks

tokenizeValue [] toks word = Value(read . reverse $ word) : toks
tokenizeValue (x : xs) toks word
    | isDigit x = tokenizeValue xs toks $ x : word
    | otherwise = tokenize' (x : xs) $ Value(read . reverse $ word) : toks

data Expr = Var String | Const Double | Sum Expr Expr | Diff Expr Expr |
        Prod Expr Expr | Quo Expr Expr deriving(Show, Eq)

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

reduce (Sum e (Const 0)) = reduce e
reduce (Sum (Const 0) e) = reduce e
reduce (Sum (Const a) (Const b)) = Const $ a + b
reduce (Sum a b) =
    let a' = reduce a
        b' = reduce b
        e = Sum a' b'
    -- Only reduce again if we successfully reduced one of the terms.
    -- Otherwise we'll get stuck in an infinite loop of failed reductions.
    in if a' /= a || b' /= b then reduce e else e
reduce (Diff e (Const 0)) = reduce e
reduce (Diff (Const a) (Const b)) = Const $ a - b
reduce (Diff a b) =
    let a' = reduce a
        b' = reduce b
        e = Diff a' b'
    -- Only reduce again if we successfully reduced one of the terms.
    -- Otherwise we'll get stuck in an infinite loop of failed reductions.
    in if a' /= a || b' /= b then reduce e else e
reduce (Prod e (Const 0)) = Const 0
reduce (Prod (Const 0) e) = Const 0
reduce (Prod e (Const 1)) = reduce e
reduce (Prod (Const 1) e) = reduce e
reduce (Prod (Const a) (Const b)) = Const $ a * b
reduce (Prod a b) =
    let a' = reduce a
        b' = reduce b
        e = Prod a' b'
    -- Only reduce again if we successfully reduced one of the terms.
    -- Otherwise we'll get stuck in an infinite loop of failed reductions.
    in if a' /= a || b' /= b then reduce e else e
reduce (Quo (Const 0) e) = Const 0
reduce (Quo e (Const 1)) = reduce e
reduce (Quo (Const a) (Const b)) = Const $ a / b
reduce (Quo a b) =
    let a' = reduce a
        b' = reduce b
        e = Quo a' b'
    -- Only reduce again if we successfully reduced one of the terms.
    -- Otherwise we'll get stuck in an infinite loop of failed reductions.
    in if a' /= a || b' /= b then reduce e else e
reduce e = e
