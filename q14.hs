q14 :: [a] -> [a]
q14 = concat . fmap (\x->[x,x]) 
q14' = foldr (\x acc -> x:x:acc) []
