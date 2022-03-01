import System.IO

main = do
    handle <- openFile "somefile" ReadMode 
    contents <- hGetContents handle
    putStr contents
    hClose handle