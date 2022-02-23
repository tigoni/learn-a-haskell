import Data.Char (toUpper)
-- putStrLn takes a string and returns an I/O action that has the result type of ()
-- this is an empty turple also called a unit.

-- An IO action performed will carry out an action with side effects (reading, printing)
-- and will contain some kind of return value inside it. () is like void in java
    -- main =  putStrLn "hello world"

--An IO action is performed when it is given the name of main and the program is run.
-- do can be used to group several IO actions together

mainOld = do
        putStrLn "Hello what's your first name?"
        firstName <- getLine
        putStrLn "What your second name?"
        lastName <- getLine
        let bigFirstName = map toUpper firstName
            bigLastname = map toUpper lastName
        putStrLn $ "Hey " ++ bigFirstName ++ " " ++ bigLastname ++ " how are you?"

-- IO actions are only performed if they are given a name of main of they are inside a bigger IO action
--block composed with an do block. A do block can be used to group together IO actions. 
-- The resulting IO action can then be used in another do block. Finally all blocks have to fall into main to 
-- be processed.



--- program to continously read a line and print out the same line with the words reversed 
-- the program stops when a blank line is input
main = do
    line <- getLine
    if null line 
        then return ()
        else do
            putStrLn $ reverseWords line
            main
reverseWords :: String -> String
reverseWords = unwords . map reverse . words