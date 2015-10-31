data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

q61 :: Tree a -> Int
q61 Empty = 0
q61 (Branch _ Empty Empty) = 1
q61 (Branch _ l r) = q61 l + q61 r

q62 :: (Show a) => Tree a -> [a]
q62 Empty = []
q62 (Branch x Empty Empty) = [x]
q62 (Branch _ l r) = q62 l ++ q62 r

q62' :: (Show a) => Int -> Tree a -> [a]
q62' 0 _ = []
q62' _ Empty = []
q62' 1 (Branch x _ _) = [x]
q62' n (Branch _ l r) = q62' (n-1) l ++ q62' (n-1) r

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty)) (Branch 2 Empty Empty)
