data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

preList :: (Show a) => Tree a -> [a]
preList Empty = []
preList (Branch x l r) = [x] ++ (preList l) ++ (preList r)

inList :: (Show a) => Tree a -> [a]
inList Empty = []
inList (Branch x l r) = (inList l) ++ [x] ++  (inList r)

preInTree :: (Eq a, Show a) => [a] -> [a] -> Tree a
preInTree [] [] = Empty
preInTree (x1:prxs) inxs = let (inxl,inx:inxr) = break (==x1)  inxs;
                               (prxl,prxr) = break (`elem` inxr) prxs   
                           in Branch x1 (preInTree prxl inxl) (preInTree prxr inxr)

q671 :: (Show a) => Tree a -> String
q671 Empty = ""
q671 (Branch x l r) = show x ++ "(" ++ (q671 l) ++ ", " ++ (q671 r) ++ ")"

q672 :: (Monad m) => String -> m (Tree Char)
q672 "" = return Empty
q672 [x] = return (Branch x Empty Empty)
q672 xs = helper xs >>= (\("",t) -> return t)
helper (x:xs) |x == ',' || x == ')' = return (x:xs, Empty)
helper (x:y:xs) | y == '(' = do 
                                (',':xs', l) <- helper xs
                                (')':xs'', r) <- helper xs'
                                return (xs'', Branch x l r)
                | y == ',' || y == ')' = return (y:xs, Branch x Empty Empty)
helper _ = fail "bad parse!"
