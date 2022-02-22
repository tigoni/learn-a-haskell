-- Function takes a string and checks if every letter is part of the alphabet ie a pangram
-- Example: "The quick brown fox jumps over the lazy dog" is a pangram

isPangram :: String -> [Bool]
isPangram str =  map (\x (find (==\x)) str) (['0'..'9'] ++ [' '] ++ [','] ++ ['.'] ++ ['A'..'Z'] ++ ['a'..'z']) 
