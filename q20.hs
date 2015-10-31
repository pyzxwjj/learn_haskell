q20 :: Int -> [a] -> (a, [a])
q20 1 (x:xs) = (x, xs)
q20 n (x:xs) = (removed_element, x:unremoved_list)
    where (removed_element, unremoved_list) = q20 (n-1) xs
q20' :: Int -> [a] -> (Maybe a, [a])
q20' _ [] = (Nothing, [])
q20' 0 xs = (Nothing, xs)
q20' 1 (x:xs) = (Just x, xs)
q20' n (x:xs) = (removed_element, x:unremoved_list)
    where (removed_element, unremoved_list) = q20' (n-1) xs
