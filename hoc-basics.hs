--Officially Haskell functions take just one paramter. Any extra parameters supplied to a function
-- makes it curried function.Currying breaks down a function that takes multiple parameters into a 
-- series of functions that take one parameter each and return the another function of a single argument
-- for the final function, the actual result is returned.

--Currying is a function of many arguments which is rewritten such that it takes the first argument and returns
-- a function which in turn uses the remaining arguments and returns the value. This kind of functions are called
-- higher order functions.

-- Currying is translating a function from callable as f(a, b, c) into callable as f(a)(b)(c).

multThree :: (Num a) => a -> a -> a -> a
-- multThree :: (Num a) => a -> (a -> (a -> a)) -- function type can also be written this way since the function
-- is curried by Haskell
multThree x y z = x * y * z

multTwoWithNine = multThree 9

