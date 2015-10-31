import System.Random
q23 :: [a] -> Int -> IO [a]
q23 xs n = do
            gen <- getStdGen
            let a = take n $ randomRs (0, length xs-1) gen  
            return $ fmap (\a -> xs!!a) a 
