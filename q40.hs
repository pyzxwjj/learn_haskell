q40 :: Int -> (Int, Int)
q40 n 
    | odd n = error "This is a odd number!"
    | otherwise = helper n 2
        where helper n nend  
                 |n == nend = error "not find" 
                 |otherwise = if and [q31 nend, q31 (n-nend)] then (nend, n-nend) else helper n (nend+1)
q31 :: Int -> Bool
q31 x = foldr (\y acc -> if x `mod` y == 0 then False else acc) True [2..floor . sqrt $ fromIntegral x]

