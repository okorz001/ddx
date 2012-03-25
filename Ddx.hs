module Ddx (DdxMaybe(..), Token(..), Expr(..), tokenize, parse, derive, reduce)
    where

import Data.Char
import Data.List

newtype DdxMaybe a = DdxMaybe { getEither :: Either String a }
    deriving(Show)

-- Convenience function for creating a DdxMaybe from an error string.
ddxError = DdxMaybe . Left

instance Monad DdxMaybe where
    return x = DdxMaybe $ Right x

    (DdxMaybe (Right x)) >>= f = f x
    -- TODO: Can this be expressed with less words?
    -- This is what I really want, but it fails:
    --      err >>= _ = err
    (DdxMaybe (Left err)) >>= _ = DdxMaybe $ Left err

    fail = ddxError

data Token = Name String | Val Double | Add | Sub | Mult | Div
    deriving(Show, Eq)

tokenize str = tokenize' str []

-- Identify the start of new tokens. If it's a simple token (i.e. single
-- character) we can add it directly to the token list.
tokenize' [] toks = return $ reverse toks
tokenize' (x : xs) toks
    | isSpace x  = tokenize' xs toks
    | isLetter x = tokenizeName xs toks [x]
    | isDigit x  = tokenizeVal xs toks [x]
    | x == '+'   = tokenize' xs $ Add : toks
    | x == '-'   = tokenize' xs $ Sub : toks
    | x == '*'   = tokenize' xs $ Mult : toks
    | x == '/'   = tokenize' xs $ Div : toks
    | otherwise  = ddxError $ "Unrecognized symbol: " ++ [x]

pushName word toks = Name(reverse word) : toks

tokenizeName [] toks word = tokenize' [] $ pushName word toks
tokenizeName (x : xs) toks word
    | isLetter x = tokenizeName xs toks $ x : word
    | otherwise  = tokenize' (x : xs) $ pushName word toks

pushVal word toks = Val(read . reverse $ word) : toks

tokenizeVal [] toks word = tokenize' [] $ pushVal word toks
tokenizeVal (x : xs) toks word
    | isDigit x = tokenizeVal xs toks $ x : word
    | otherwise = tokenize' (x : xs) $ pushVal word toks

data Expr = Var String | Const Double | Sum Expr Expr | Diff Expr Expr |
        Prod Expr Expr | Quo Expr Expr deriving(Show, Eq)

-- Split a list on the first occurance of a key.
findAndSplit keys items =
    case span (`notElem` keys) items of
        (left, []) -> (left, Nothing, [])
        (left, (key : right)) -> (left, Just key, right)

-- The earlier tokens are parsed, the looser the binding.
parse toks = parseSumDiff toks

parseSumDiff toks =
    case findAndSplit [Add, Sub] toks of
        (left, (Just Add), right) -> do
            left' <- parse left
            right' <- parse right
            return $ Sum left' right'
        (left, (Just Sub), right) -> do
            left' <- parse left
            right' <- parse right
            return $ Diff left' right'
        _ -> parseProdQuo toks

parseProdQuo toks =
    case findAndSplit [Mult, Div] toks of
        (left, (Just Mult), right) -> do
            left' <- parse left
            right' <- parse right
            return $ Prod left' right'
        (left, (Just Div), right) -> do
            left' <- parse left
            right' <- parse right
            return $ Quo left' right'
        _ -> parseConstVar toks

-- Const and Var are terminal. There should not be any other tokens.
parseConstVar [Val x] = return $ Const x
parseConstVar [Name x] = return $ Var x
parseConstVar toks = ddxError $ "Unexpected token(s): " ++ show toks

derive _ (Const _) = Const 0
-- Cannot use pattern matching to check for equality.
derive x (Var y) = Const $ if x == y then 1 else 0
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
