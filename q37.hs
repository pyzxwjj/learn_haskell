import Data.List
q31 :: Int -> Bool
q31 x = foldr (\y acc -> if x `mod` y == 0 then False else acc) True [2..floor . sqrt $ fromIntegral x]
q35 :: Int -> [Int]
q35 n
    | q31 n = [n]
    | otherwise =  findx:q35 (n `div` findx)
        where findx = head $ filter (\x -> mod n x == 0) [2..n]
q36 :: Int -> [(Int, Int)]
q36 n = map (\x -> (head x,length x)) . group . q35 $ n
q37 :: Int -> Int
q37 n = foldr (\(x,xn) acc -> acc*helper x xn) 1 $ q36 n 
    where helper x xn = round $ (xi-1)*xi**(xni-1) 
            where xi = myconv x
                  xni = myconv xn
          myconv x = fromInteger . toInteger $ x
