q17 :: [a] -> Int -> ([a], [a])
q17 [] _ = ([], [])
q17 xs 0 = ([], xs)
q17 xs n = (myfun1 n xs, myfun2 n xs)
    where myfun1 0 xs = []
          myfun1 n (x:xs) = x:myfun1 (n-1) xs
          myfun2 0 xs = xs
          myfun2 n (x:xs) = myfun2 (n-1) xs
