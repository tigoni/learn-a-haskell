-- putStrLn takes a string and returns an I/O action that has the result type of ()
-- this is an empty turple also called a unit.

-- An IO action performed will carry out an action with side effects (reading, printing)
-- and will contain some kind of return value inside it. () is like void in java
    -- main =  putStrLn "hello world"

--An IO action is performed when it is given the name of main and the program is run.
-- do can be used to group several IO actions together

main = do
    putStrLn "Hello what's your name"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", great to meet you")

-- IO actions are only performed if they are given a name of main of they are inside a bigger IO action
--block composed with an do block. A do block can be used to group together IO actions. 
-- The resulting IO action can then be used in another do block. Finally all blocks have to fall into main to 
-- be processed.

