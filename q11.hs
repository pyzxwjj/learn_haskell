import Data.List
data MyChar = Multiple Int Char | Single Char deriving (Show, Eq)
q11 :: String -> [MyChar]
q11 string = let
                strGroup = group string
                myfun y@(x:xs) = if length xs == 0  then Single x else Multiple (length y) x 
             in map myfun strGroup
