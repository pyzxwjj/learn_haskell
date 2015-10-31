q31 :: Int -> Bool
q31 x = foldr (\y acc -> if x `mod` y == 0 then False else acc) True [2..floor . sqrt $ fromIntegral x]
