import Data.List
data MyChar = Multiple Int Char | Single Char deriving (Show, Eq)
q11 :: String -> [MyChar]
q11 string = let
                strGroup = group string
                myfun y@(x:xs) = if length xs == 0  then Single x else Multiple (length y) x 
             in map myfun strGroup
q12 :: [MyChar] -> String
q12 [] = []
q12 (x:xs) = case x of
                Single y -> y: q12 xs
                Multiple n y -> replicate n y ++  q12 xs
