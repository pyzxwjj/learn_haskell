data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show)
q59 :: (Show a) => a -> Integer -> [Tree a]
q59 a (-1) = []
q59 a 0 = [Empty]
q59 a n = [Branch a tl tr |  (ln, rn) <- [(n-1,n-2), (n-2,n-1), (n-1, n-1)],
                             tl <- q59 a ln,
                             tr <- q59 a rn]
hbalTree x = map fst . hbalTree'
    where hbalTree' 0 = [(Empty, 0)]
          hbalTree' 1 = [(Branch x Empty Empty, 1)]
          hbalTree' n =
                let t = hbalTree' (n-2) ++ hbalTree' (n-1)
                in  [(Branch x lb rb, h) | (lb,lh) <- t, (rb,rh) <- t
                                         , let h = 1 + max lh rh, h == n]
