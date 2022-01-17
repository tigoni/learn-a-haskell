import Data.List

--Function that uses imported function from List to show unique items in a list
--nub is defined in list and weeds out duplicates in a list
numUniques :: (Eq a) => [a] -> Int
--using function composition
--numUniques = length . nub 

numUniques xs = length (nub xs)
