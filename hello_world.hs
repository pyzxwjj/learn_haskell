main :: IO ()
main = do
       putStrLn "hello, world!"
       a <- getLine
       putStrLn a
       return ()
