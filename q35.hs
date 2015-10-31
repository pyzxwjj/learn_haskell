q31 :: Int -> Bool
q31 x = foldr (\y acc -> if x `mod` y == 0 then False else acc) True [2..floor . sqrt $ fromIntegral x]
q35 :: Int -> [Int]
q35 n
    | q31 n = [n]
    | otherwise =  findx:q35 (n `div` findx)
        where findx = head $ filter (\x -> mod n x == 0) [2..n]
