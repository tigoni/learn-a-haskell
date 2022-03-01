import System.IO
import Data.Char

main' = do
    handle <- openFile "somefile" ReadMode 
    contents <- hGetContents handle
    putStr contents
    hClose handle

-- using withFile function: takes a Filepath and IOMode and a function that takes a Handle and returns an IO action
-- it returns an IO action that opens the file does something with that file and closes it. The result in the final 
-- IO action is the same as that returned by the function in the last parameter.
-- withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a

main'' = do
    withFile "./workfiles/somefile" AppendMode (\handle -> do 
        contents <- hGetContents handle
        putStr contents)

--using readFile
main''' = do
    contents <- readFile "./workfiles/somefile"
    putStr contents
    putStr "\n"

-- writting to a file using writeFile
main_ = do
    contents <- readFile "./workfiles/somefile"
    writeFile "./workfiles/booksFile" (map toUpper contents) 

--appending to a file
main = do
    toDoItem <- getLine
    appendFile "./workfiles/toDoFile" ("[]" ++ toDoItem ++ "\n")