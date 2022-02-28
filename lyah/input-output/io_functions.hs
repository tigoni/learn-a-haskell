import Control.Monad
import Data.Char

-- main = do
--         print True
--         print 2
--         print "Cars"
--         print 3.2
--         print [3,7,9]
-- --print can use anything that is an instance of Show. it calls show (to stringify the value) and then prints it
--        c <- getChar
--        if c /= ' '
--                then do
--                        putChar c
--                        main
--                else return ()

--using when
-- Takes a boolean and an IO. If boolean is True it returns the same IO function. If false,-- it returns an empty IO.
--   putStrLn "-----Using when---"
--   c <- getChar
--   when (c /= ' ') $ do
--     putChar c
--     main

--sequence: takes a list of IO actions and returns an IO action that will perform those actions one after another.
-- the result contained in that IO action will be a list of results of all the IO actions that were performed
--   putStrLn "-----Using sequence ---"
--   a <- getLine
--   b <- getLine
--   print [a, b]

--same as
--   putStrLn "---Aternative sequence approach---"
--   rs <- sequence [getLine, getLine, getLine] --makes an IO action to perform getLine three times
--   print rs

--creating an IO action that prints a list
--   sequence (map print [1,2,3,4,5])

-- --mpaM takes a function and a list maps the function over the list and then sequences it
--   mapM print [1,2,3]

--forever: takes an IO action and retutns an IO action that repeats the IO action forever
main = forever $ do
  putStr "Give some input: "
  x <- getLine
  putStrLn $ map toUpper x
