{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import System.Environment
import System.Directory
import System.IO
import Data.List
import Distribution.PackageDescription (CondTree(condTreeComponents))

dispatch :: [(String, [String] -> IO ())]
--an association list (key-value mapping) that will hold the name-function mapping
dispatch = [("add", add),("view", view)]

main = do 
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

add :: [String] -> IO ()
add [fileName, toDoItem] = appendFile fileName (toDoItem ++ "\n")

view :: [String] -> IO()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [1..] todoTasks
    putStr $ unlines numberedTasks