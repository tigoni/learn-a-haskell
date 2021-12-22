module TopicReviews where

sumOfList :: (Num a) => [a] -> a

sumOfList [] = 0 
sumOfList (x:xs) = x + sumOfList xs

-- rank strings 
flickRank :: String -> Int 

flickRank movie | movie == "A" = 2
		| movie == "B" = 1
		| otherwise = 0
