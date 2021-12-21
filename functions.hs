module StartWithFunctions () where 
{-
 - Functions in Haskell do not have to be in any order
 - functions cannot begin with upper-case letters.
 - Parentheses are only used to disambiguate expressions when multiple expr are used
 -
 - 
-}

-- define a function that double a paramter supplied 
doubleMe x = x + x


doubleUs num1 num2 = num1*2 + num2*2

--defining doubleUs using another function defined before.
-- a common pattern in haskell is to compose complex functions from simpler ones
doubleUs2 x y = doubleMe x + doubleMe y

squareMe num = num * num
sqrAndDbl value = squareMe value  + doubleMe value   

-- Function that double only a small number (less than 10)
doubleSmallNumber x = if x > 10 then x else x*2

--Scenarios for parenthesis
func1 a b = a + b
func2 c d = c * d

--func3 is a call of func1 which will use the return value of func2 for its second argument
func3  x y z = (func1 x (func2 y z))
