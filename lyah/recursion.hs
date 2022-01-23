module StartRecursion where

-- Get the biggest in a list of things that can be ordered
maximum' :: (Ord a ) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
   | x > maxTail = x
   | otherwise = maxTail
   where maxTail = maximum' xs

-- Rewritting the function using the max function
maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x:xs) = max x (maximum'' xs)


-- replicate' takes an Int and an element and returns a list of repetitions of the elemenent 
-- replicate' 3 5 -> [5,5,5]
-- uses guard conditions instead of pattern matching above

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x:replicate' (n-1) x


--take' Takes a certain number of elements from a list
-- take' 3 [5,4,3,2,1] returns [5,4,3]
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

--Reverse a list
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--Haskell supports infinite lists so recursion does not have to 
-- have an edge condition
-- repeat' 3 --> prints an endless list of 3s
--take' 12 (repeat' 4) From  the list of infinite 4s, take the first 12
repeat' :: a -> [a]
repeat' x = x:repeat' x

--combine two lists recursively 
-- pair up the heads and then the tails
-- zip' [1,2,4] [6,8] -> [(1,6),(2,8)]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- quicksort algorithm 
quicksort :: (Ord a ) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
  let smallerSorted = quicksort [a | a <- xs, a <= x ]
      biggerSorted = quicksort [a | a <- xs, a > x ]
  in smallerSorted ++ [x] ++ biggerSorted
