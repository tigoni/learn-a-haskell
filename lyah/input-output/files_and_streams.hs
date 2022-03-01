
import Control.Monad
import Data.Char

--program takes input and prints only shortLines
-- main = do
--     contents <- getContents
--     putStr (shortLinesOnly contents)

-- shortLinesOnly :: String -> String
-- shortLinesOnly input = 
--     let allLines = lines input
--         shortLines = filter (\line -> length line < 10) allLines
--         result = unlines shortLines
--     in  result


--shorter alternative
-- main = do interact $ unlines . filter ((<10) .length) . lines

--interact: takes a function of type String -> String -> IO ()

--declare function
countChars :: String -> String
countChars xs = "total chars are " ++ show (length xs)

main = do
        putStrLn "Enter a string to get the count"
        interact countChars