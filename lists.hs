--working with lists
numbers = [1,2,3]
joinedNumbers = numbers ++ [5,7,9]

--strings are character lists
str1 = ['H','e','l','l','o']
str2 = "Hello"
stringsEqual = str1 == str2

--joing lists 
result = ['w','o'] ++ ['o','t']

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


anotherList = [[4,6,7],[20,10,30],[3,7,11]]
target = anotherList !! 1 !! 2 -- target is 30

--lists are compared in lexicological order
p = [2,5,7] > [1,9,5] --True
d = [1,8,7] > [1,9,5] --False
u = [9,5,7] > [9,5,5] --True

let items = [10,20,20,40,50]
head items -- 10
tail items -- removes the list's head. returns [20,20,40,50]
init items -- removes the list's tail (last element) 
length items -- number of items in the list
null items -- checks if list is empty
reverse items -- reverses the list
take 2 items --extracts the specified number of items from the list --returns [10,20]
drop 3 items --removes the number of items on the list specified
maximum items --return the largest item in the list -- 50
minimum items --return the smallest item in the list -- 50
sum items -- total sum of items in the list
product items -- product of items in the list
10 `elem` items -- detemines if 10 is in the list and returns True/False



