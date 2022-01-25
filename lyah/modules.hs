module WorkingWithModules where 

import Data.List
-- import Data.List (nub, sort)  --import some select functions in a module
-- import Data.List hiding (nub)  --import all other functions in module except nub
-- import qualified Data.Map --import functions from Map that will be called in a qualified form such that conflict with other similar functions can be avoided.
--import qualified Data.Map as M --every function called from this module has to be qualified with M eg. M.filter
--
--
--
--Function that uses imported function from List to show unique items in a list
--nub is defined in data.List and weeds out duplicates in a list
numUniques :: (Eq a) => [a] -> Int
--using function composition
--numUniques = length . nub 
numUniques xs = length (nub xs)



{-
 - Data.List Functions
-}

res = intersperse '.' "TEST" 
--"T.E.S.T"

-- takes a list of lists and a list and inserts lists into the list of lists and flattens
flattened = intercalate "cat" ["dog","cow"] 
--"dogcatcow"

-- transpose: flip rows and cols in a 2D matrix (list of lists)
flippedMatrix = transpose [[1,2,3],[4,5,6],[7,8,9]]

-- concat: flattens a list of lists to just a list
concat [[5,3,7],[9,2,2]]
--[5,3,7,9,2,2]

-- concatMap: map function to a list and flatten the list
concatMap (replicate 4) [1..5]
-- [1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5]

--and: take list of boolean values and return True if all in list are True
and $ map (>4) [5,6,7,8]
--True

--any/anyAll: takes a predicate and check if any or all elements in a list satisfy the predicate.(works similar to `and` above)
any (==4) [3,5,7,2,8]
--False

all (`elem` ['A'..'Z']) "HEYGUYSGreetings"
--False


