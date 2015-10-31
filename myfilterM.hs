myfilterM :: (Monad m) => (x -> m Bool) -> [x] -> m [x]
myfilterM f [] = return []
myfilterM f (x:xs) = do
                   a <- f x
                   as <- myfilterM f xs
                   if a then
                       return (x:as)
                   else
                       return as


