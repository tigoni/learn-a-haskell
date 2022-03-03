main = do
    toDoItem <- getLine
    appendFile "./workfiles/todo.txt" ("[]" ++ toDoItem ++ "\n")