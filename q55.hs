import Data.List
data Tree a = Empty | Tree a (Tree a) (Tree a) deriving (Show)

q55 :: Integer -> [Tree Char]
q55 0 = [Empty]
q55 n = let (p, r) = quotRem (n-1) 2
        in [Tree 'x' tl tr | i <- [p..(p+r)],
                             tl <- q55 i,
                             tr <- q55 (n-i-1)] 
