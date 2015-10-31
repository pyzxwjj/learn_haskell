data Tree a = Node a [Tree a] deriving (Show)
tree5 = (Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]])
q73 :: Tree Char -> String
q73 (Node x []) = ' ':[x]
q73 (Node x xs) = '(':x:foldr (\y acc -> " " ++ q73 y ++ acc) ")" xs 

