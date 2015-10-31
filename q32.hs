q32 :: Int -> Int -> Int
q32 a b = mygcd (abs a) (abs b)
    where mygcd a 0 = a
          mygcd a b = mygcd b (mod a b)
