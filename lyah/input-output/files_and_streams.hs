
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

--interact: takes a function of type String -> String and returns an IO action. That IO action takes some input and 
--applies the function on it and prints out the function's results.

--usefull SO answer on how interact works: https://stackoverflow.com/questions/16799755/haskell-interact-function
-- main = interact (unlines .map countChars.lines)

--declare function
countChars :: String -> String
countChars xs = show $ length xs


--code that checks if an input line is a palindrome or not
respondPalindromes = unlines .map (\xs -> if isPalindrome xs then "palindrome" else "not palindrome") . lines 
    where isPalindrome xs = xs == reverse xs

main = interact respondPalindromes