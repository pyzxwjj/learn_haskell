import Control.Monad
import Data.Char
data Tree a = Node a [Tree a] deriving (Show)
newtype P a = P { runP :: String -> Maybe (a, String) }
 
instance Monad P where
        return x = P $ \ s -> Just (x, s)
        P v >>= f = P $ \ s -> do
                (x, s') <- v s
                runP (f x) s'
 
instance MonadPlus P where
        mzero = P $ \ _ -> Nothing
        P u `mplus` P v = P $ \ s -> u s `mplus` v s
 
charP :: P Char
charP = P view_list
  where view_list [] = Nothing
        view_list (c:cs) = Just (c, cs)
 
literalP :: Char -> P ()
literalP c = do { c' <- charP; guard (c == c') }
 
spaceP :: P ()
spaceP = P (\ s -> Just ((), dropWhile isSpace s))

data Syntax a = Syntax {
                display :: a -> String,
                parse :: P a
        }


(<*>) :: Syntax a -> Syntax b -> Syntax (a,b)
a <*> b = Syntax {
                display = \ (va,vb) -> display a va ++ display b vb,
                parse = liftM2 (,) (parse a) (parse b)
        }

(<|>) :: Syntax a -> Syntax b -> Syntax (Either a b)
a <|> b = Syntax {
                display = either (display a) (display b),
                parse = liftM Left (parse a) `mplus` liftM Right (parse b)
        }
 
char :: Syntax Char
char = Syntax return charP
 
literal :: Char -> Syntax ()
literal c = Syntax (const [c]) (literalP c)
 
space :: Syntax ()
space = Syntax (const " ") spaceP
 
iso :: (a -> b) -> (b -> a) -> Syntax a -> Syntax b
iso a_to_b b_to_a a = Syntax {
                display = display a . b_to_a,
                parse = liftM a_to_b (parse a)
        }

(*>) :: Syntax () -> Syntax a -> Syntax a
p *> q = iso snd ((,) ()) (p <*> q)
 
-- list of a's, followed by finish
list :: Syntax a -> Syntax () -> Syntax [a]
list a finish = iso toList fromList (finish <|> (a <*> list a finish))
  where toList (Left _) = []
        toList (Right (x, xs)) = x:xs
        fromList [] = Left ()
        fromList (x:xs) = Right (x, xs)

df :: Syntax (Tree Char)
df = iso toTree fromTree (char <*> list df (literal '^'))
  where toTree (x, ts) = Node x ts
        fromTree (Node x ts) = (x, ts)

tree5 = Node 'a' [Node 'f' [Node 'g' []],Node 'c' [],Node 'b' [Node 'd' [],Node 'e' []]]
