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

--iterate: take a function and a start value and repeatedly apply that function each resulting value
take 10 $ iterate (*2) 10
--10,20,40,80,160,320,640,1280,2560,5120 

--splitAt: take a number and a list. slit the list at the index of the number and give the resulting lists in a turple.
splitAt 5 "tesaracting"
--("tesar","acting")

--takeWhile: takes elements from a list while a predicate holds. Stops when an element that does not satisfy the predicate is encountered.
takeWhile (>2) [5,3,4,2,7,6,1]
--[5,3,4]

takeWhile (/='t') "This is not a test"
--"This is no"

--dropWhile: works in the opposite way to takeWhile. drops all the elements when the predicate is true. Returns the rest of the  list when the predicate turns false
dropWhile (/=' ') "today is a good day"
--" is a good day"

--sort: sorts an element of typeclass Ord
sort [3,5,2,8]
--[2,3,5,8]

--group: groups a list into adjacent sublists if they are equal
group [1,1,2,2,3,4,4][1,1,2,2,3,4,4]
[[1,1],[2,2],[3],[4,4],[1],[6]]

--inits: returns all initial segments of the list argument recursively. Shortest first.
inits "cowbell"
["","c","co","cow","cowb","cowbe","cowbel","cowbell"]

--tails: returns all the final segments of the list argument. longest first
tails "cowbell"
["cowbells","owbells","wbells","bells","ells","lls","ls","s",""]

--using tails to create a function that searches a substring in a string
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack = 
	let nlen = length needle
	in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

--partition: takes a list and predicate and returns a pair of lists. First list has all elements that satisfy the predicate, second list has all that do not satisfy the predicate.
partition (>3) [4,6,7,2,8,3,1,1,6,3]
([4,6,7,8,6],[2,3,1,1,3])

