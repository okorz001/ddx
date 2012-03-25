import Control.Monad
import Ddx

-- Ugh, this feel dirty.
tryDerive str = do
    expr <- tokenize str >>= parse
    return $ reduce . derive "x" $ expr

main = forever $ do
    putStr "Type an expression: (^C to quit.)\n"
    contents <- getLine
    case tryDerive contents of
        (DdxMaybe (Left err)) -> print err
        (DdxMaybe (Right e)) -> print e
