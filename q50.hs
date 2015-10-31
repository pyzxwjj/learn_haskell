import Data.List
import Data.Ord
data Htree a = Leaf a | Branch (Htree a) (Htree a) deriving (Show)

q50 :: (Ord w, Ord a, Num w) => [(a,w)] -> [(a, [Char])]
q50 xs = sortBy (comparing fst) $ myflatten $ makeHtree $ sortBy (comparing fst) $map (\x -> (snd x,Leaf (fst x))) xs
    where makeHtree [(_, b)] = b
          makeHtree ((w1, b1):(w2, b2):ys) = makeHtree $ insertBy (comparing fst) (( w1+w2), Branch b1 b2) ys
          myflatten (Leaf a) = [(a, [])]
          myflatten (Branch l r) = [(a,'0':abit)|(a,abit)  <- myflatten l] ++ [(a,'1':abit)|(a,abit) <- myflatten r]
