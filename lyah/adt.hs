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


-- the data keyword creates a new data type and specifies all the values it can possibly be.-- This is also called a sum type since the values of the type are summed togetther to make the whole type
--
-- data for animal can be any of the values specified and nothing else. These are valled value constructors
data Animal = Giraffe
            | Elephant
            | Tiger
            | Flea

type Zoo = [Animal]

localZoo :: Zoo

localZoo = [Elephant
            , Tiger
            , Tiger
            , Giraffe
            , Elephant
            ]

adviceOnFood :: Animal -> String
adviceOnFood animal = 
    case animal of
         Giraffe -> "Acacia branches"
         Tiger -> "Beef"
         Elephant -> "A lot of bushes"

adviceOnFeedAll :: Zoo -> [String]
adviceOnFeedAll [] = []
adviceOnFeedAll (x:xs) = adviceOnFood x : adviceOnFeedAll xs


--value constructors can take value params to produce new values
data Shape = Circle Float Float Float 
           | Rectangle Float Float Float Float
     deriving Show

--type constructors can take types as parameters to produce new types
data Maybe a = Nothing | Just a


data Person' = Person' { firstName :: String
                       , lastName :: String
                       , age :: Int
                     } 
                     deriving (Eq, Show, Read)

getAge' :: Person' -> Int
getAge' (Person' _ _ a ) = a



