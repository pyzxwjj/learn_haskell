q19 :: [a] -> Int -> [a]
q19 xs n = let
                pos = if n >= 0 then n else length xs + n
                (xfnd, xsnd) = splitAt pos xs
           in   xsnd ++ xfnd 
