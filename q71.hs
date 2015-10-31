data Tree a = Node a [Tree a] deriving (Show)
q71 x = helper (0, x)
helper (n, (Node _ [])) = n
helper (n, Node _ xs) = n + foldr (\x acc -> acc + helper (n+1, x)) 0 xs
tree5 = Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]

