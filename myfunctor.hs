class Myfunctor f where
    myfmap :: f a -> (a -> b) -> f b

instance Myfunctor Maybe where
    myfmap Nothing f = Nothing
    myfmap (Just x) f = Just (f x)
instance Myfunctor ((->) r) where
    myfmap = flip (.)
