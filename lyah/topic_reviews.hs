module TopicReviews where

sumOfList :: (Num a) => [a] -> a

sumOfList [] = 0 
sumOfList (x:xs) = x + sumOfList xs

-- rank strings 
flickRank :: String -> Int 

flickRank movie | movie == "A" = 2
  | movie == "B" = 1
  | otherwise = 0

--using recursion to sum up a list
sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--reversing a list
reverse' :: (Num a) => [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

--take a number and repeat it infinitely into a list
repeat' :: a -> [a]
repeat' x = x:repeat' x  


--find an element in a list
elem' :: (Eq a) => a -> [a] -> Bool
elem'  a [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = a `elem` xs

