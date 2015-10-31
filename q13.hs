import Data.List
data MyChar = Multiple Int Char | Single Char deriving (Show, Eq)
q13 :: String -> [MyChar]
q13 = foldr myfun []
        where myfun new [] = [Single new] 
              myfun new acc@(x:xs) = if myfun3 new x then (Single new):acc else myfun2 x xs
              myfun2 (Single x) xs = Multiple 2 x : xs
              myfun2 (Multiple n x) xs = Multiple (n+1) x : xs
              myfun3 x (Single y) = x /= y
              myfun3 x (Multiple _ y) = x /= y
q12 :: [MyChar] -> String
q12 [] = []
q12 (x:xs) = case x of
                Single y -> y: q12 xs
                Multiple n y -> replicate n y ++  q12 xs
