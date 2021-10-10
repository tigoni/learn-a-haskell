
--Add type declarations to functions

removeLowerClass :: [Char] -> [Char] --method signatuure maps a string to a string
--removeLowerClass :: String -> String  -- a list os chars is a string so this works too
removeLowerClass arg = [c | c <- arg, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int --functions adds threee integers and returns an Integer
addThree x y z = x + y + z


factorial :: Int -> Int
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2  * pi * r
