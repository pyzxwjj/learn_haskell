data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)

treeToList :: Tree Char -> String
treeToList Empty = "."
treeToList (Branch x l r) = [x] ++ (treeToList l) ++ (treeToList r)

listToTree :: (Monad m) => String -> m (Tree Char)

listToTree xs = helper xs >>= (\("", t) -> return t)

helper (x:y:z:xs) | y == '.' && z == '.' = return (xs, (Branch x Empty Empty))
helper (x:xs) | x == '.' = return (xs, Empty)
              | otherwise = do (xs', l) <- helper xs
                               (xs'', r) <- helper xs'
                               return (xs'', Branch x l r)
helper _ = fail "parse error!"
