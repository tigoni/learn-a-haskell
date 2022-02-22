
module Shapes 
(Point(..)
, Shape(..)
, Shape'(..)
, surface
, surface'
, move
, baseRect
, baseCircle
) where

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving Show

--function takes a Shape and returns the surface
-- pattern matching can be used on value constructors
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2 --value constructors are functions 
surface (Rectangle x1 y1 x2 y2)  = (abs $ x2 - x1) * (abs $ y2 - y1)

-- using a map (takes a function and a list and applies the function to every member of the list to produce a list) and partial function application,  we can make a list of concentric circles
result = map (Circle 12 5) [2,3,4,5]
--[Circle 12.0 5.0 2.0,Circle 12.0 5.0 3.0,Circle 12.0 5.0 4.0,Circle 12.0 5.0 5.0]

-- refactor Shape to use Point types
data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

--refactor the surface function
surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2 
surface' (Rectangle' (Point x1 y1) (Point x2 y2))  = (abs $ x2 - x1) * (abs $ y2 - y1)
result2 = surface' $ Circle' (Point 3 7 ) 5
--78.53982

-- moving the shape
-- function takes the shape, amount to move in x-axis, amount to move in y-axis and returns a new shape located in the new space.
move :: Shape' -> Float -> Float -> Shape'
move (Circle' (Point x y) r ) a b = Circle' (Point (x+a) (y+b)) r
move (Rectangle' (Point x1 y1) (Point x2 y2)) a b = Rectangle' (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- move (Rectangle' (Point 0 1) (Point 0 7)) 5 9
-- Rectangle' (Point 5.0 10.0) (Point 5.0 16.0)

-- abstracting points to a separate function so we do not have to use them creating shape. The base shapes start at point 0 0 on the x,y axis and move can be called on those points
baseCircle :: Float -> Shape'
baseCircle r = Circle' (Point 0 0) r

baseRect :: Float -> Float -> Shape'
baseRect width height = Rectangle' (Point 0 0) (Point width height)

newShapeLocation = move (baseRect 30 70) 100 200





-- Record Syntax: a way of reducing effort in accessing data fields from a custom data type
-- Example: Create a data type Person that has the fields: firstname, lastname, age, height, and phonenumber.
data Person = Person String String Int Float String deriving (Show)

-- To access the fields of this data type, one would need to write functions for each field as follows:
person_firstname :: Person -> String
person_firstname (Person firstname _ _ _ _ ) = firstname

person_age :: Person -> Int
person_age (Person _ _ age _ _ ) = age

-- For all fields, this would be cumbersome. Haskell solves this using record syntax.

data Person' = Person' { firstname :: String
                       , lastname :: String
                       , age :: Int
                       , height :: Float
                       , phonenumber :: String
                       } deriving (Show)
-- This way, Haskell makes functions that look up the fields. So that functions: firrstname, lastrname, age, height and phonenumber now exist
p = Person' "Jane" "Mwas" 34 53.23 "+254756333220"
fname = firstname p 
lname = lastname p

-- record syntax also changes how the string representation of the data type is displayedG
-- with first datatype:
p2 = Person "Jose" "Proe" 78 62.45 "+254782999332"
--Person "Jose" "Proe" 78 62.45 "+254782999332"

--with record syntax
p3 = Person' {firstname="Jane", lastname="Mwas", age=34, height=53.23, phonenumber="+254756333220"}
-- Person' {firstname = "Jane", lastname = "Mwas", age = 34, height = 53.23, phonenumber = "+254756333220"}

-- although we can still create a person in the more common method. The string representation remains the same.
p4444 = Person' "Jane" "Mwas" 34 53.23 "+254756333220"


--Type parameters
--data Maybe a = Nothing | Just a

-- A type constructor can take up types as parameters to produce new types. In the above example, a is a type parameter. The type Maybe when used will end up holding either a Nothing value or a Maybe Int, Maybe Char, Maybe String etc

--x = Just 'T'
-- :t x is x :: Maybe Char

safeDiv :: (Integral a) => a -> a -> Maybe a
safeDiv a b = if b == 0 then Nothing else Just $ div a b 

-- Box is an abstract container that can hold any other type
data Box a = Box a deriving Show 

--Type synonyms: making type aliases for readability in code and documentation
type UserName = String
type Password = String 
type Users = [(UserName, Password)]

userExists :: UserName -> Password -> Users -> Bool
userExists username password userlist = (username, password) `elem` userlist

-- Parametrized type synonyms:  can be generalised so that they work on more types
type UserList k v = [(k,v)]

-- this will work when k and v are Int, String
