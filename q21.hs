q21 :: a -> [a] ->  Int -> [a]
q21 insert_element xs 1 = insert_element:xs
q21 insert_element (x:xs) n = x:q21 insert_element xs (n-1)
