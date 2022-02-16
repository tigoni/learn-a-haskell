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
 | x < a = treeElem x left
 | x > a = treeElem x right
