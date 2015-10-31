data Tree a = Node a [Tree a] deriving (Show)
tree5 = (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])

q72 (Node x []) = [x]
q72 (Node x xs) = foldr (\y acc -> q72 y ++  acc) [x] xs

