import Control.Monad
import Ddx

main = forever $ do
    putStr "Type an expression: (^C to quit.)\n"
    contents <- getLine
    print $ reduce . derive "x" . parse . tokenize $ contents
