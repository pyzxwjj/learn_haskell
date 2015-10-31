import Data.List
q26 :: Int -> [a] -> [[a]]
q26 0 _  = [[]]
q26 n xs = [y:xs' | y:ys <- tails xs, xs' <- q26 (n-1) ys]
