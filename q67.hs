data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)
treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) =
        x : '(' : treeToString l ++ "," ++ treeToString r ++ ")"
 
stringToTree :: (Monad m) => String -> m (Tree Char)
stringToTree "" = return Empty
stringToTree [x] = return $ Branch x Empty Empty
stringToTree str = tfs str >>= \ ("", t) -> return t
    where tfs a@(x:xs) | x == ',' || x == ')' = return (a, Empty)
          tfs (x:y:xs)
                | y == ',' || y == ')' = return (y:xs, Branch x Empty Empty)
                | y == '(' = do (',':xs', l) <- tfs xs
                                (')':xs'', r) <- tfs xs'
                                return $ (xs'', Branch x l r)
          tfs _ = fail "bad parse"

























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
