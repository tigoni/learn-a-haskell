
import Control.Monad
import Data.Char

main = forever $ do
  putStr "Give some input: "
  x <- getLine
  putStrLn $ map toUpper x
