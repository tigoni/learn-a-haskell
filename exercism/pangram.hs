-- Function takes a string and checks if every letter is part of the alphabet ie a pangram
-- Example: "The quick brown fox jumps over the lazy dog" is a pangram
import Data.List
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram str = all (/=Nothing) . (\x -> find (==x) (map (toLower str))) $ [' '] ++ ['.'] ++ ['a'..'z'] 
