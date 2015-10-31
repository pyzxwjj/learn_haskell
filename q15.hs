q15 :: [a] -> Int -> [a]
q15 [] _ = []
q15 (x:xs) n = myreplicate n x ++ q15 xs n
                where myreplicate 0 x = []
                      myreplicate n x = x : myreplicate (n-1) x
