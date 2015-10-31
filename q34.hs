q33 :: Int -> Int -> Bool
q33 a b = if mygcd (abs a) (abs b) == 1 then True else False
    where mygcd a 0 = a
          mygcd a b = mygcd b (mod a b)
q34 :: Int -> Int
q34 1 = 1
q34 n = length $ filter (\x -> q33 n x) [1..(n-1)]
