q28 :: [[a]] -> [[a]]
q28 = my_sort
    where 
        my_sort [] = []
        my_sort (x:xs) = my_sort (filter (\y -> length y <= length x) xs) ++ [x] ++ my_sort (filter (\y -> length y > length x) xs)
