module StartingFuncSyntax where

--pattern matching functions: This is a way of defining multiple function bodies to work on data that matches specific patterns.  It can be used on any data type.
-- patterns check if the input has a particular form. 

--When sayMe is called, pattern matching will be used to determine the correct function to be called.
--In this case, calling sayMe with a 1-5 will output a string of the same integral. Anything else will be mapped to x and the string 'Not a number' will be printed.
sayMe :: (Integral a ) => a -> String
sayMe 1 = "One"
sayMe 2 = "Two"
sayMe 3 = "Three"
sayMe 4 = "Four"

sayMe x = "Not a number"


--use pattern matching to define a factorial function
factorial :: (Integral a ) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

--redefine the head function using pattern matching
head' :: [a] -> a
head' [] = error "Cannot get head of empty list"
head' (x:_) = x 


--length function
len [] = 0
len (x:t) = 1 + len t


--use case for pattern matching
len2 list = 
  case list of 
    [] -> 0
    (_:t) -> 1 + len2 t

--append lists
append [] l = l --if first list is empty just return the list
append (x:t) l = x : append t l

--adding vectors using pattern matching
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

--custom function to extract component from a triple (Haskell only provides fst and snd to work with pairs)
--see :info fst or :t fst 
third :: (a, b, c ) -> c
third (_, _, z) = z

--pattern matching lists in functions
add :: [a] -> a 
add [] = error "Not possible"
add (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "The list has many elements, the first two are: " ++ show x ++ " and " ++ show y

--implementing length function using pattern matching and recursion
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs


--summing items in a list
sum' :: (Num a ) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs -- sum of list is the sum of the head plus sum of the rest of the list

--as patterns:  a way of breaking something up according to a pattern an binding it to the names while keeping a reference to the whole thing. Uses a @ infront of the pattern. Useful to avoid repeating a pattern inside a function body.
capital :: String -> String
capital "" = "Empty string, Not allowed" 
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

--guards: Tests whether some property of a value (or several of them) are true or false. Guards test the boolean conditions of input. Guards basically replace if-then-else statements which also evaluate based on boolean conditions.

bestCapSize :: (RealFloat a) => a -> String 
bestCapSize size
  | size <= 3.5 = "Too small for you"
  | size <= 4.5 = "May fit but a bit tight"
  | size <= 5.5 = "This is the best size"
  | size <= 5.0 = "A bit oversize"
  | otherwise = "That's too big!"


coinCirculation :: (RealFloat a) => a -> a -> String
coinCirculation marketCap price
  | marketCap / price <= 400 = "Too low supply"
  | marketCap / price <= 600 = "Moderate supply"
  | marketCap / price <= 800 = "High supply"
  | otherwise = "Don't even consider"


-- a max function
max' :: (Ord a) => a -> a -> a
max' a b | a > b = a 
         | otherwise = b



--using where bindings to avoid repaeting the guard conditions


coinCirculation' :: (RealFloat a) => a -> a -> String
coinCirculation' marketCap price
  | result <= 400 = "Too low supply"
  | result <= 600 = "Moderate supply"
  | result <= 800 = "High supply"
  | otherwise = "Don't even consider"
  where result = marketCap / price

-- the where binding can also include the conditions defined as variables
coinCirculation'' :: (RealFloat a) => a -> a -> String
coinCirculation'' marketCap price
  | result <= low = "Too low supply"
  | result <= moderate = "Moderate supply"
  | result <= high = "High supply"
  | otherwise = "Don't even consider"
  where result = marketCap / price
        low = 400
        moderate = 600
        high = 800

--Let it be: similar to where bindings. However they can be used to bind to variables anywhere and are expressions themselves. They do not span accross guard conditions.
--Let it be bindings let us bind variables anywhere and are expressions themselves. Theay are local annd do not span accross guards
-- function to get the surface area of a cylinder given its height and radius
cylinder :: (RealFloat a) => a -> a -> a 
cylinder r h = 
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea

result = (let (a,b,c) = (1,2,3) in a+b+c) * 100

--binding several variables 
output = (let a = 100; b = 200; c= 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

--using let to define the bmi function
calcBmis :: (RealFloat a ) => [(a,a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w /h ^2]


--case expressions
--pattern matching I used above is actually syntactic sugar for case expressions
head''' :: [a] -> a
head''' xs = case xs of [] -> error "No head for empty lists"
                        (x:_) -> x

--unlike pattern matching for function parameters, case expressions can be used anywhere
describeList :: [a] -> String 
describeList xs = "The list is " ++ case xs of [] -> "empty"
                                               [x] -> "a singleton list"
                                               xs -> "a longer list"
