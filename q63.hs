data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)
q63 :: (Show a) => a -> Int -> Tree a
q63 a 0 = Empty
q63 a n = let (p, r) = quotRem (n-1) 2 in Branch a (q63 a (p+r)) (q63 a p)

