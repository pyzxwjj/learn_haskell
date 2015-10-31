primelist :: Integer -> [Integer]
primelist 2 = [2]
primelist 3 = [3, 2]
primelist 4 = [3,2]
primelist n 
        | all (\x -> n `mod` x /= 0) $ primelist $ (floor.sqrt.fromInteger) n  = n: primelist (n-1)
        | otherwise = primelist (n-1)

isPrime :: Integer -> Bool
isPrime n = all (\x -> n `mod` x /= 0) $ primelist $ (floor.sqrt.fromInteger) n 

q31 :: Int -> Bool
q31 x = foldr (\y acc -> if x `mod` y == 0 then False else acc) True [2..floor . sqrt $ fromIntegral x]

primelist' :: [Int]
primelist' = 2:3:next primelist' 3  
next plist x = let  xnext = head $ filter (\x->mod0 x $ takeWhile (\y -> let xmax = (floor.sqrt.fromIntegral) x in y <= xmax) plist) [(x+1)..]  in xnext:next primelist' xnext

mod0 :: Int -> [Int] -> Bool
mod0 x xl = all (\n -> x `mod` n /= 0) xl 
