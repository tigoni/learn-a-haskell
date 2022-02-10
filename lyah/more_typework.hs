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
search :: (Eq k) => k -> AssocList k v -> Maybe v
search  key items = lookup key items

result = search 1 anotherDict
result2 = search 5 anotherDict
