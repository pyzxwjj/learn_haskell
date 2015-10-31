import Data.List
data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)
q55 :: Integer -> [Tree Char]
q55 0 = [Empty]
q55 n = let (p, r) = quotRem (n-1) 2
        in [Branch 'x' tl tr | i <- [p..(p+r)],
                             tl <- q55 i,
                             tr <- q55 (n-i-1)] 

isMirror :: (Eq a) => Tree a -> Tree a -> Bool
isMirror Empty Empty = True
isMirror (Branch a1 al ar) (Branch b1 bl br) = and [a1 == b1, isMirror al br, isMirror ar bl]
isMirror _ _ = False

q56 :: (Eq a) => Tree a -> Bool
q56 x = isMirror x x

isMirror' :: (Eq a) => Tree a -> Tree a -> Bool
isMirror' Empty Empty = True
isMirror' (Branch a1 al ar) (Branch b1 bl br) = and [isMirror' al br, isMirror' ar bl]
isMirror' _ _ = False

q56' :: (Eq a) => Tree a -> Bool
q56' x = isMirror' x x

q57 :: (Ord a) => [a] -> Tree a
q57 xs = foldl helper Empty xs
            where
helper Empty x = (Branch x Empty Empty)
helper (Branch bx bl br) x = case compare x bx of
                                LT -> (Branch bx (helper bl x) br) 
                                GT -> (Branch bx bl (helper br x))
                                EQ -> (Branch bx bl br) 
q57' :: (Ord a) => [a] -> Bool
q57' = q56'.q57

q58 :: Integer -> [Tree Char]
q58 n = if even n then
            []
        else
            [Branch 'x' l (reverseTree l) | l <- q55 (div n 2)]
        where reverseTree Empty = Empty
              reverseTree (Branch x l r) = Branch x (reverseTree r) (reverseTree l)
