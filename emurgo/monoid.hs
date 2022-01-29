data Tree a = Empty
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)


tree1 = Empty
tree2 = Node (Node Empty 3 Empty) 5 (Node Empty 7 Empty)


treeSize :: Tree a -> Int
treeSize Empty = 0
treeSize (Node l n r ) = 1 + (treeSize l) + (treeSize r)


treeSum :: Tree Int -> Int 
treeSum Empty = 0
treeSum (Node l n r ) = 1 + (treeSum l) + (treeSum r)


treeDepth :: Tree a -> Int
treeDepth Empty = 0
treeDepth (node l n r) = 1 + max(treeDepth l ) max (treeDepth r) 

flatten = Tree a -> [a]
flatten Empty = []
flatten (Node left Node right) = (flatten left) ++ [n] + (flatten right) 
