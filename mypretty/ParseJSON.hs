module ParseJSON where
import SimpleJSON
import Data.Char (isSpace, isDigit)
import Control.Applicative
--import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.Map as Map

preParse :: String -> (JValue, String)
preParse ('\"':xs) = toJValue . parseString $ xs
preParse ('[':xs) = parseArray xs
preParse ('{':xs) = parseObject xs
preParse xs | (isDigit . head) xs = parseNum xs
            | otherwise = parseBool xs

parse :: String -> (JValue, String)
parse xs = preParse . rmSpace $ xs

parseString :: String -> (String, String)
parseString ('\"':xs) = ("", xs) 
parseString (x:xs) | isSpace x = parseString xs
                   | otherwise = let (s', xs') = parseString xs in (x:s', xs')
toJValue :: (String, String) -> (JValue, String)
toJValue (v, s) = (JString v, s)

parseNum :: String -> (JValue, String)
parseNum xs = ((JNumber . read) jnum, ss)
            where (jnum, ss) = span ((||) <$> isDigit <*> (=='.')) xs

parseBool :: String -> (JValue, String)
parseBool xs | take 4 b == "true" = (JBool True, last b:ss)
             | b == "false" = (JBool False, ss)
             | otherwise = error "matching failed!"
            where (b, ss) = splitAt 5 xs

parseArray :: String -> (JValue, String)
parseArray (']':xs) = (JArray [], xs)
parseArray (',':xs) = parseArray xs 
parseArray xs = let (jv, ss) = parse xs
                    (JArray jvs, ss') = parseArray . rmSpace $ ss
                in (JArray (jv:jvs), ss')

parseObject:: String -> (JValue, String)
parseObject ('}':xs) = (JObject Map.empty, xs)
parseObject (',':xs) = parseObject xs
parseObject xs = let ((jkey, jvalue), ss) = parsePair . rmSpace $ xs
                     (JObject jpairs, ss') = parseObject . rmSpace $ ss
                 in (JObject (Map.insert jkey jvalue jpairs), ss')

parsePair :: String -> ((String, JValue), String)
parsePair xs = let (jp1, ss) = parseString . tail $ xs
                   (jp2, ss') = parse . tail . dropWhile (/=':')  $ ss
               in ((jp1, jp2), ss')


rmSpace = dropWhile (isSpace)
