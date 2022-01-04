-- Enumeration types
-- This declares a type 'Thing' with 3 data constructors. They are the only values of type thing 
module StartADTs where

data Thing = Shoe 
           | Ship 
           | Cabbage
  deriving Show

shoe = Shoe
cabb = Cabbage

--some functions on Thing using pattern matching
isSmall :: Thing -> Bool

-- Ship is the only large thing so..
isSmall Ship = False 
isSmall _ = True 

-- More general ADT
-- FailableDouble is a type that has two data construnctors 
data FailableDouble = Failure 
                    | OK Double
           deriving Show

--show 
a = Failure
b = OK 3.6

main = print (a,b)

-- Making use of the type
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- More than 1 arg in data constructors
-- Store person's name and age and gender using two data constructors
data Person = Person String Int 
            | Person2 String Int Char
 deriving Show

jack :: Person
jack = Person "jack" 30

stan :: Person
stan = Person "Stan" 29


alen :: Person
alen = Person2 "Alen" 29 'F'

-- get age of a Person
getAge :: Person -> Int
getAge (Person _ a) = a
getAge (Person2 _ a _) = a




