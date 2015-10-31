data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

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
