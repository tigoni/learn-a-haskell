module WorkingWithModules where 

import Data.List

--Function that uses imported function from List to show unique items in a list
--nub is defined in list and weeds out duplicates in a list
numUniques :: (Eq a) => [a] -> Int
--using function composition
--numUniques = length . nub 
numUniques xs = length (nub xs)

--Data.List: (Some functions already included in Prelude)
-- Functions:
res = intersperse '.' "TEST" 
--"T.E.S.T"

-- takes a list of lists and a list and inserts lists into the list of lists and flattens
flattened = intercalate "cat" ["dog","cow"] 

-- flip rows and cols in a 2D matrix (list of lists)
flippedMatrix = transpose [[1,2,3],[4,5,6],[7,8,9]]
