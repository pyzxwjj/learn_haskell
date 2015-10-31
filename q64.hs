data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

q64 :: (Show a) -> Tree a -> Tree (a, Int, Int)

myflatten Empty = []
myflatten (Branch x l r) = x:
