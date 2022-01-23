func1 :: [Int] -> Int
func1 [] = 1
func1 (x:xs) 
  | even x = (x - 2) * func1 xs
  | otherwise = func1 xs


-- writting func1 using foldl 
func2 :: [Int] -> Int 
func2 xs = foldl (\acc x -> acc * x) 1 (filter (even) xs)
-- using function composition
func2 xs = foldl (\acc x -> acc * x) 1 . filter (even) xs

