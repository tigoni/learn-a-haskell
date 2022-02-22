module CustomAndTypeClasses where

import Prelude hiding (Just, Nothing)
import Data.Either
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
--type synonyms: same as adding alias to a type for easier readability and documentation
type String' = [Char]

-- 
type PhoneBook = [(String, String)]

myPhoneBook :: PhoneBook

myPhoneBook = [ ("Allex","0723747444")
               ,("Bob","07863332182")
               ,("Chris","0732833291")]

type PhoneNumber = String
type Name = String 
--this conveys better information on the use of the type
type PhoneBook' = [(Name, PhoneNumber)]

-- type synonyms can be parametrized (genericized)
type AssocList k v = [(k,v)]
-- AssocList is a type constructor that takes two types and produces a concrete type 

someList :: AssocList Int Int
someList = [(2,3)]

anotherDict :: AssocList Int String
anotherDict = [(1,"KCR-324R"),(2,"KDV-389P")] 

--define a function that get a value from an association list using a key
-- by using the assoclist, search can work with any given concrete type used for the list
search :: (Eq k) => k -> AssocList k v -> Maybe v
search  key items = lookup key items

result = search 1 anotherDict
result2 = search 5 anotherDict

--Recursive data types
-- A data constructor can have fields that are of the same type therefore creating recursive data types.
-- A list [3] is syntactic sugar for 5:[]. List [4,5,6] is also 4:(5:(6:[])). It shows a list can be an element joined to 
-- another list which can be empty or another list.
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

--Cons is a constructor that is just another word for ':'.
-- In lists ':' is a constructor that takes a value and another list and returns a list.
-- Functions can be made to be automaticaly infix by composing them of special characters 

infixr 5 :-:
data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

--First a fixity declaration is made (infixr 5 :-:) This states how tightly the operator binds and whether its left or right associative.

-- let x = 4 :-: 2 :-: Empty'
-- 4 :-: (2 :-: Empty')

--defining a new function to add the list
infixr 5 .++
(.++) :: List' a -> List' a -> List' a 
Empty' .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

a = 5 :-: 3 :-: 8 :-: Empty'
b = 0 :-: 9 :-: Empty'
results = a .++ b
-- 5 :-: (3 :-: (8 :-: (0 :-: (9 :-: Empty'))))


-- Implementing a binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

--utility function just to make a singleton tree
singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

-- function to insert a value into a given tree
treeInsert :: (Ord a ) => a -> Tree a -> Tree a 
treeInsert x EmptyTree = singleton  x
treeInsert x (Node a left right)
 | x == a = Node x left right
 | x < a = Node a (treeInsert x left) right
 | x > a = Node a left (treeInsert x right)


--search the tree for a value
treeElem :: (Ord a ) => a -> Tree a -> Bool 
treeElem x EmptyTree = False 
treeElem x (Node a left right)
 | x == a = True

-- typeclasses 102
-- making custom typeclasses

class Eq' a where
    (<==>) :: a -> a -> Bool
    (</=>) :: a -> a -> Bool 
    x <==> y =  not (x </=> y)
    x </=> y =  not (x <==> y)

-- using the typeclass on a type
data TrafficLight = Red | Green | Yellow

instance Eq' TrafficLight where
    Red <==> Red = True 
    Green <==> Green = True 
    Yellow <==> Yellow = True 
    _ <==> _ = False 

    --no need to implement the (/=) function since (==) was defined interms of (/=) and vide-versa
x = Red
y = Green
sameType = x <==> y 

-- show can also be defined in custom (although the default prints the values names too)
instance Show TrafficLight where
    show Red = "R"
    show Green = "G"
    show Yellow = "Y"

color = show y 


--Yes-no typeclass: creating a typeclass similar to javascripts truthy and false expression values
class YesNo a where
    yesno :: a -> Bool 


instance YesNo Int where
    yesno 0 = False 
    yesno _ = True

value = yesno (3 :: Int) --True
value2 = yesno (0 :: Int) --False


instance YesNo [a] where
    yesno [] = False
    yesno _ = True


data CustomMaybe a = Just a | Nothing deriving Show
instance YesNo (CustomMaybe a) where
    yesno (Just _) = True
    yesno Nothing = False 

--Functor: A typeclass for things that can be mapped over
-- The list type is a member of the Functor typeclass 
-- A Functor class defines a function fmap' that takes a function that is applied on a 
-- wrapped value and returns a wrapped value
--Any type that can be thought of as a container can become a member of the functor typeclass
class Functor' f where 
    fmap' :: (a -> b) -> f a -> f b

-- make CustomMaybe a member of Functor typeclass. So it has to implement the fmap' function
instance Functor' CustomMaybe where
    fmap' func (Just a ) = Just (func a )
    fmap' func Nothing = Nothing 

--adding Functor to a Tree type
instance Functor' Tree where
    fmap' f EmptyTree = EmptyTree
    fmap' f (Node x leftsub rightsub) = Node (f x) (fmap' f leftsub) (fmap' f rightsub)

--Either:  A type that represents values with two possiblities
computedResult = Right 100 :: Either String Int
computeError =  Left "Error Occured!":: Either String Int

-- The fmap' from the Functor instance will apply the supplied function tot he values contained in a Right and
-- ignore the Left values
r = fmap (*3) computedResult
--Right 300

e = fmap (*3) computeError 
--Left "Error Occured!"