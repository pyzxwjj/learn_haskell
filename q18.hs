q18 :: [a] -> Int -> Int -> [a]
q18 xs a b = let 
                (_, x1) = splitAt (a - 1) xs
                (x2, _) = splitAt (b -  a + 1) x1
             in x2
q18' xs a b = let 
                (x1, _) = splitAt b xs
                (_, x2) = splitAt (a-1) x1
              in x2
