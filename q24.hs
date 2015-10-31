import System.Random 
import Data.List
q24 :: Int -> Int -> IO [Int]
q24 n last_one = do 
                    gen <- getStdGen
                    return $ take n $ nub $ randomRs (1, last_one) gen
