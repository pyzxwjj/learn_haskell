import Data.List

solveRPN :: (Num a, Read a) => String -> a
solveRPN expression = head (foldl foldfun [] $ words expression)
    where   foldfun (x:y:xs) "*" = (x*y):xs
            foldfun (x:y:xs) "+" = (x+y):xs
            foldfun (x:y:xs) "-" = (y-x):xs
            foldfun (x:xs) "p" = (x^2):xs
            foldfun xs number = read number:xs
