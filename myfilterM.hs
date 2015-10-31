myfilterM :: (Monad m) => (x -> m Bool) -> [x] -> m [x]
myfilterM f [] = return []
myfilterM f (x:xs) = do
                   a <- f x
                   as <- myfilterM f xs
                   if a then
                       return (x:as)
                   else
                       return as

myfoldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
myfoldM f acc [] = return acc
myfoldM f acc (x:xs) = do
                       m1 <- f acc x
                       myfoldM f m1 xs
