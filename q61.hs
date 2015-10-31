data Tree a = Empty | Branch a (Tree a) (Tree a)

q61 :: Tree a -> Int
q61 Empty = 0
q61 (Branch _ Empty Empty) = 1
q61 (Branch _ l r) = q61 l + q61 r
