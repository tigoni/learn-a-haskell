module StartWithHigherOrderFunctions where 
{- Officially Haskell functions take just one paramter. Any extra parameters supplied to a function
-- makes it curried function.Currying breaks down a function that takes multiple parameters into a 
-- series of functions that take one parameter each and return the another function of a single argument
-- for the final function, the actual result is returned.

--Currying is a function of many arguments which is rewritten such that it takes the first argument and returns
-- a function which in turn uses the remaining arguments and returns the value. This kind of functions are called
-- higher order functions.

-- Currying is translating a function from callable as f(a, b, c) into callable as f(a)(b)(c).
-}

multThree :: (Num a) => a -> a -> a -> a
-- multThree :: (Num a) => a -> (a -> (a -> a)) -- function type can also be written this way since the functioni is curried by Haskell
multThree x y z = x * y * z

multTwoWithNine = multThree 9


-- examples of HOCs from Youtube Video:
-- https://www.youtube.com/watch?v=ccExc6rrUN8
-- Function 'doIt' takes two params: a function and a value 
-- It applies the function to the value

--define function to pass to 'doIt' as a value
add1 :: Int -> Int
add1 x = x + 1

doIt :: (a -> b) -> a -> b
doIt add1 y = add1 y  
res = doIt add1 3 
-- call 'doIt' with function as an argument
--outputs 4 



--the function argument can be an anonymous function
-- prints 8
x = doIt (\x -> x * 2 ) 4 


--Some Haskell higher order functions
-- Map: takes a function and a list and applies the function to that list to produce another list

-- [13,8,5,9,10]
withFiveAddedList = map (\x -> x + 5) [8,3,1,4,5] 

--Filter: takes a function (a predicate since it alwys retuns a Bool) and a list and returns a list initialized based on the results of the predicate
numbersGreaterThanTwo = filter (\x -> x > 2) [9,4,1,4,0] -- filter all numbers greater than 2 [9,4,4] 

-- Partial Function Application and Currying

--func :: a -> b -> c -> d

{- can be re-written as a curried function. Actually functions that take multiple arguments  do not exist in Haskell but rather functions take a single argument that returns another function or an end result.
-}
-- func :: a -> (b -> (c -> d))

-- Using currying to define function add
-- All 3 definitions are equivalent
add :: Int -> Int -> Int 

add x y = x + y

--add x = (\y -> x + y)

--add = (\x -> (\y -> x + y))


{- Function Function Application
 Since funtions in Haskell work with single arguments, passing a function less arguments that it expects will simply cause the function to return another function needing as many arguments as had been left out. This is call paartial function application and it allows us to create functions on the fly and pass them to others and also seed them with some data
-}

--min :: (Ord a ) => a -> a -> a
-- min  :: (Ord a ) => a -> (a -> a -> a) 
-- Returns end result
leastNumber = min 4 5 
-- returns a function expecting a single argument (the 4 passed here will be hardcoded arleady)
funcX = min 4 

--Partial application of infix functions
divideByTen :: (Floating a) => a -> a 
divideByTen = (/10)


-- Function as param. applyTwice takes a function as a param  and applies it twice to the second param  
applyTwice :: (a -> a) -> a -> a
applyTwice funcParam x = funcParam (funcParam x)

concatText = applyTwice ("Boom " ++ ) "Bang"
results = applyTwice (+3) 5
myList = applyTwice (3:)[1]


--Implementing zipWith: Takes a function and two lists and applies that function to the corresponding list elements
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs ) (y : ys) = f x y : zipWith' f xs ys

-- Using zipWith' 
combined = zipWith' (+) [1,2,3] [4,5,6]
--get the max elements in two lists
maxInLists = zipWith' max [2,4,6] [1,3,7]

actors = zipWith' (++) ["Denzel", "Al", "Samuel"] [" Washington", " Pacino ", " L. Jackson"]

-- Function that takes a functions and returns a function with params flipped
flip' :: (a -> b -> c ) -> (b -> a -> c)
flip' f = g
     where g x y = f y x

--Implementing map: takes a function and applies it to every item in a list
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' func (x:xs) = func x : map' func xs

doubledNumbers = map' (*2) [1,2,3,4,5]
squared = map' (map' (^2)) [[1,2],[2,3]]

-- Filter: function that takes a predicatei (a function that determined if something is true or false and returns a boolean value) and and a list and returns a list that satisfies the predicate

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
   | f x = x : filter' f xs 
   | otherwise = filter' f xs

-- Using the filter' function
greaterThanThree = filter' (>3) [1,3,8,5,1,2,4,6]

equalToThree = filter' (==3) [1,2,3,4,5]

evenNumbersList = filter' even [1..10]

lowerCased = filter' (`elem` ['a'..'z']) "Today Is A Day In December"

listsNotEmpty = let notNull x = not (null x) in filter' notNull [[1,2,4],[],[3],[6,8]] 


-- Implementing quicksort using filter 
quicksort' :: (Ord a ) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = 
    let smallerSorted = quicksort' (filter (<=x) xs)
        biggerSorted = quicksort' (filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted   

-- Get the largest number under 100,000 divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- Lambdas
-- Anonymous functions used where the function is needed only once. Usually needed for passing to a higher order function. 

-- Using a lambda to add elements in a list of turples and return list
summedUpTurples = map(\(a,b) -> a + b ) [(1,2),(3,5),(6,3),(2,6)]

--Lambdas can take any number of parameters
fancyCalculation = zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

-- Folds:  Utility functions introduced as a way of handling the common pattern of recursing through lists with pattern matching. A fold function takes a binary function, a starting value and a list to fold up. The binary function takes two parameters. (thr starting value (accumulator) and the first (or last) element of the list and produces a new  accumulator value). This function is called with the new  accumulator and list element until the the accumulator remains.
--}

-- foldl: folds up a list from the left
summ :: (Num a) => [a] -> a
summ xs = foldl (\accum x -> accum + x) 0 xs

--since functions are curried the above implementation can be re-written as:
summ' :: (Num a) => [a] -> a
summ' = foldl (+) 0


-- Implementing elem using foldl
elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys =  foldl (\acc x -> if x == y then True else acc) False ys

-- foldr: reduces a list from the right. The binary function parameters should be in the order: current value and then
-- accumulator which is opposite to foldl
--Implementing a mapping function using foldr
doOperationToAGivenList :: (a -> b) -> [a] -> [b]
doOperationToAGivenList f xs = foldr(\x acc -> f x : acc) [] xs

-- Folds can be used where a list is traversed at least once and something is being returned based on that.
-- foldl1 and foldr1 are similar but do not need to be provided with a start value. They assume the first/last element
-- the list to be the stating value and starts the fold with the element next to it.
--


-- Function composition: Two functions both working on the parameter x (f x)(g x) can be re-written as f (g(x))
-- Function composition can be used to replace lambdas for more clear code

map (\x -> negate (abs x)) [5, -3, -6, 7]
--can be rewritten as :

map (negate . abs ) [5, -3, -6, 7]

-- Another ex of composition
abs (negate (sum (tail [1..5])))

-- can be re-written as
(abs . negate . sum . tail) [1..5] 
