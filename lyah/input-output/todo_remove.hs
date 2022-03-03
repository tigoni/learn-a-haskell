import System.IO
import System.Directory
import Data.List

main = do 
    handle <- openFile "./workfiles/todo.txt" ReadMode 
    (tempName, tempHandle) <- openTempFile "./workfiles" "temp"
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " _ " ++ line) [1..] todoTasks
    putStrLn  "these are the todo items"
    putStr $ unlines numberedTasks
    putStrLn "which ones do you wan to delete"
    numberString <- getLine
    let number = read numberString
        newtodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newtodoItems
    hClose handle
    hClose tempHandle
    removeFile "./workfiles/todo.txt"
    renameFile tempName "./workfiles/todo.txt"