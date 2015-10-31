data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

q61 :: Tree a -> Int
q61 Empty = 0
q61 (Branch _ Empty Empty) = 1
q61 (Branch _ l r) = q61 l + q61 r

q62 :: (Show a) => Tree a -> [a]
q62 Empty = []
q62 (Branch x Empty Empty) = [x]
q62 (Branch _ l r) = q62 l ++ q62 r
