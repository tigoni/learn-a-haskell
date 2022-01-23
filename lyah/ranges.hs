module StartingWithRanges () where
--ranges: lists that are arithematic sequences that can be enumerated
--eg. 1-10, a-z ...
--
myRange = [1..20]
alphabet = ['a'..'z'] -- letters a - z

primeNumbers = [1,3..20] -- sets a range of all prime numbers between 1 and 20

reverseNumbers = [10,9..0] -- 10 to 0

--ranges can be used to make infinite lists

-- function takes a list and cycles though it infinitely. Since Haskell is lazy it 
-- will only evaluate the list when what is requred from that list is specified
-- cycle [1,2,3] 
firstitems x list= take x (cycle list) -- print the first 15 elmenets of this inifinte list
