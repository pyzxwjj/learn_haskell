q16 :: [a] -> Int -> [a]
q16 xs n = myfun (n-1) (n-1) xs 
        where myfun ns n [] = []
              myfun ns 0 (x:xs) = myfun ns ns xs 
              myfun ns n (x:xs) = x: myfun ns (n-1) xs

