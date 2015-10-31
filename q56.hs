data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Eq, Show)

isMirror :: (Eq a) => Tree a -> Tree a -> Bool
isMirror Empty Empty = True
isMirror (Branch a1 al ar) (Branch b1 bl br) = and [a1 == b1, isMirror al br, isMirror ar bl]
isMirror _ _ = False

q56 :: (Eq a) => Tree a -> Bool
q56 x = isMirror x x
