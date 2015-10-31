import System.Random
import Data.List
q25 :: [a] -> IO [a]
q25 xs = do
           gen <- getStdGen
           let tmp = take (length xs). nub $ randomRs (0, length xs -1) gen 
           return $ fmap (\x -> xs!!x) tmp
q25' xs = do
            gen <- getStdGen
            let ele = [0..length xs - 1]
            return $ fmap (\x -> xs!!x) $ rand ele gen
          where
            rand [] gen = []
            rand ele gen = randomx:rand randomxs gen'
                where (randorder, gen') = randomR (0, length ele - 1) gen
                      randomx = ele!!randorder 
                      randomxs = delete randomx ele
