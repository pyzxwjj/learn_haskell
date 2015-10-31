q22 :: (Eq a, Enum a) => a -> a -> [a]
q22 a b = if a/= b then a:q22 (succ a) b else [b] 
