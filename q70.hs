data Tree a = Node a [Tree a] deriving (Show)

q701 :: Tree a -> Integer
q701 (Node x []) = 1
q701 (Node x xs) = 1 + foldr (\x acc -> q701 x + acc) 0 xs


tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]

treeToString :: Tree Char -> String
treeToString (Node x []) = x:"^"
treeToString (Node x xs) = x:(foldr (\x acc -> treeToString x ++ acc) "" xs) ++ "^"

stringToTree :: String -> Tree Char
stringToTree "" = error "no tree exists!"
stringToTree xs = let (_, [x]) = helper xs in x 

--helper (x:xs) | x == '^' = (xs, [])
--              | otherwise = let (xl, nodel) = helper xs; (xl', nodel2) = helper xl in (xl', (Node x nodel):nodel2)
helper (x:y:xs) | y == '^' = Node x []
                | ohterwise = let (xl, nodel) = helper2 (y:xs) in (xl, Node x nodel)
helper _ = error "parse error!"

helper2 xall@(x:xs) | x == '^'  = (xs, [])
                    | otherwise = let (xl, nodel) = helper xall ;(xl', nodel2) = helper2 xl in (xl', nodel:nodel2)


