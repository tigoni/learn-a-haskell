-- Example code on how to use case in Haskell
--
data Choice = First String | Second String | Third | Fourth deriving (Show)

-- function to determine which case was selected
whichCase ch = 
    case ch of 
      First _ -> "first choice!"
      Second _ -> "second choice!"
      _ -> "Something else!"

option = First "My selection"
result = whichCase option 
 
