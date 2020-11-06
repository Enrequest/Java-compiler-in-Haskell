module ParserLibrary where

import Prelude hiding ((<*>), (<$>), (<$), (<*), (*>))

import MyScanner

type Parser s r = [s] -> [(r,[s])]

symbol :: Eq s => s -> Parser s s
symbol sr (se:rs) = if sr == se then [(se,rs)]  else []
symbol _  _       = []

succeed :: a  -> Parser s a
succeed r xs = [(r, xs)]

infix  7 <$>
infix  7 <$
infixl 6 <*>
infixl 6 <*
infixl 6 *>
infixr 4 <|>

(<|>) :: Parser s r -> Parser s r -> Parser s r
(p <|> q) s = p s ++ q s

(<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
(p <*> q) xs = [ (f z, zs) 
               | (f  , ys) <- p xs
               , (  z, zs) <- q ys ]

(<*) :: Parser s b -> Parser s a -> Parser s b
(p <* q) xs = [ (y, zs) 
              | (y, ys) <- p xs
              , (z, zs) <- q ys ]

(*>) :: Parser s b -> Parser s a -> Parser s a
(p *> q) xs = [ (z, zs) 
              | (y, ys) <- p xs
              , (z, zs) <- q ys ]
--infixl 6 <*>

(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) s = [ (f r, rs) | (r,rs) <- p s ]

(<$) :: b -> Parser s a -> Parser s b
(f <$ p) xs = [ (f, ys) 
              | (y, ys) <- p xs ]

option :: Parser s a -> a -> Parser s a
option p d = p <|> succeed d

many :: Parser s a -> Parser s [a]
many p = list <$> p <*> many p <|> succeed []

many1 :: Parser s a -> Parser s [a]
many1 p = list <$> p <*> many p

list x xs = x:xs

