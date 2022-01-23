--working with lists
numbers = [1,2,3]
joinedNumbers = numbers ++ [5,7,9]

--strings are character lists
str1 = ['H','e','l','l','o']
str2 = "Hello"
stringsEqual = str1 == str2

--joing lists 
result = ['w','o'] ++ ['o','t']

--Using the ++ operator to join lists is not efficient since Haskell has to gro though the 
-- entire list on the left of the ++ to determine the end. 
-- The : (Cons) opearator is more efficient since it puts something at the beggining of the
-- list

--prepending a list 
count = 5:[4, 3, 2, 1]
word = 'A':" cow moos!" ++ [' ','a'] ++ " lot!"

--getting things from the list
b = [1,2,3,4,5,6,7]
--get value in index 2 of the list
res = b !! 2 

mylist = [[1,2]]
appendList = mylist ++ [[3,4]]
anotherlist = [8]:appendList
target = anotherlist !! 2

-- get element in 2d array
anotherList = [[4,6,7],[20,10,30],[3,7,11]]
elementNeeded = anotherList !! 1 !! 2 -- target is 30

--lists are compared in lexicological order
p = [2,5,7] > [1,9,5] --True
d = [1,8,7] > [1,9,5] --False
u = [9,5,7] > [9,5,5] --True

items = [10,20,20,40,50]
firstElem = head items -- 10
listTail = tail items -- removes the list's head. returns [20,20,40,50]
listHead = init items -- removes the list's tail (last element) 
itemsCount = length items -- number of items in the list
isEmpty = null items -- checks if list is empty
reversed = reverse items -- reverses the list
duo = take 2 items --extracts the specified number of items from the list --returns [10,20]
removed = drop 3 items --removes the number of items on the list specified
max = maximum items --return the largest item in the list -- 50
min = minimum items --return the smallest item in the list -- 50
summed = sum items -- total sum of items in the list
prod = product items -- product of items in the list
isTen = 10 `elem` items -- detemines if 10 is in the list and returns True/False


--list comprehensions
-- double each item in the list of 1-10
doubledItemsInListOfTen = [x*2 | x <- [1..10]]

 --bind every item in list to x and double it, use a predicate to filter only doubles greater or equal to 12
 --output: [12,14,16,18,20]
doubledItemsInListOfTenGreaterThanOrEqualToTwelve = [x*2 | x <- [1..10], x*2 >=12 ]
 

--list comprehensions return specific lists from more general lists by applying filtering to th--original list.

-- print all odd numbers between 1 and 100
-- the output function is the part before the pipe
-- the input set is the list bound to the variable x
-- the predicate is the function/part that does the filtering, several predicates can be includ-- ed
oddNumberPrinter = [x | x <- [1..100], odd x]


--a list comprehension that multiplies each int in a list (1 to 10) with another list  
multiplierList = [x*y | x <- [1..10], y <- [10,20..100]]

--get the length of a list (just add a 1 to every current bound variable in xs then sum up )
length' xs = sum [1 | _ <- xs]

--function takes a string and removes everything except the uppercase characters
funcLowerCaser str = [c | c <- str, c `elem` ['A'..'Z']]





