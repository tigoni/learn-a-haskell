module StartTypeSystem where

{-
 - Haskell type system:
 - Strongly typed - Compiler enforces types on values and expressions. It will not coerce types - Static types - Compiler knows the type of every value and expression at compile time
 - Type inference - The Compiler can deduce automatically the types of all expressions in a progrem
--}
--
--Add type declarations to functions

removeLowerClass :: [Char] -> [Char] --method signatuure maps a string to a string
--removeLowerClass :: String -> String  -- a list os chars is a string so this works too
removeLowerClass arg = [c | c <- arg, c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int --functions adds threee integers and returns an Integer
addThree x y z = x + y + z


factorial :: Int -> Int
factorial n = product [1..n]

circumference :: Float -> Float
circumference r = 2  * pi * r

{-
 - Int - Bounded whole numbers with max (on a 32 bit machine) at 2147483647 and - 2147483647
 - Integer - Unbounded whole number. Really big numbers can be represented
 - Float - A real floating point with single precision
 - Double - Real floating point with double the precision 
 - Bool - boolean type. True of False values
 - Char - character. Denoted by single quotes
-}

--Type variables: A mechanism in haskell used to represent variables that can be of any type
-- similar to generics in other languages. Functions using type variables are called polymorphic functions-- example of such a functions are head and fst
--


--Typeclass: A sort of interface that defines specific behavior. A type that is a par tof a typeclass means it supports and implements the behavior the typeclass defines. 
--It constrains the types that can be used for a value or expression.
--Example: Eq typeclass provides an interface for equality testing. In a type where two value of that type can be compared, the type should be a member of that Eq class.
--Some basic typeclasses:
--Eq : support equality testing
--Ord: class for types that have an ordering
--Show: members of this class can be presented as strings
--Read: members of this class can have their string representation turned to the actual type
--Enum: members have sequentially ordered types. Can be enumerated
--Bounded: members have an upper and lower bound
--Num: members can act like numbers
--Integral: members are only integral whole numbers. Int and Iteger are members. 
--Floating: members include only Floating point numbers.
--
