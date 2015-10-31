gray :: Integral a => a -> [String]
gray 0 = [""]
gray n = foldr (\s acc -> ("0" ++ s):("1" ++ s):acc) [] $ gray (n-1)

q49 :: Int -> [String]
q49 n = map helper $ makebinary n 
    where helper xs = zipWith (\x1 x2 -> if x1 == x2 then '0' else '1') (0:xs) xs
          makebinary 0 = [[]] 
          makebinary n = [x:xs| x <- [0, 1], xs <- makebinary (n-1)]
