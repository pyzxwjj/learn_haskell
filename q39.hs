q31 :: Int -> Bool
q31 x = foldr (\y acc -> if x `mod` y == 0 then False else acc) True [2..floor . sqrt $ fromIntegral x]
q39 :: Int -> Int -> [Int]
q39 a b = filter q31 [a..b]
