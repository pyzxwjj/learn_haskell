-- file: ch05/Main.hs
module Main(main)  where
import SimpleJSON
import PutJSON
import ParseJSON
main = do
       txt <- getContents
       let p = renderJValue . fst . parse $ txt
       putStrLn p

