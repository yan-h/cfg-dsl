{-# LANGUAGE InstanceSigs, LambdaCase, MonadComprehensions #-}

module Parser where

import           Data.Char
import           Control.Applicative            ( Alternative
                                                , empty
                                                , (<|>)
                                                )
import           Control.Monad

newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- Always succeeds, producing a constant result. Doesn't consume the input string.
result :: a -> Parser a
result v = Parser $ \cs -> [(v, cs)]

-- Always fails, producing no result.
zero :: Parser a
zero = Parser $ const []

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \cs -> [(f a, res) | (a, res) <- parse p cs]

instance Applicative Parser where
  pure :: a -> Parser a
  pure = result

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> q = Parser $ \cs -> concat [parse (f <$> q) res | (f, res) <- parse p cs]

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \cs -> concat [parse (f v) res | (v, res) <- parse p cs]

instance MonadPlus Parser where
  mzero :: Parser a
  mzero = zero

  mplus :: Parser a -> Parser a -> Parser a
  mplus p q = Parser $ \cs -> parse p cs ++ parse q cs

instance Alternative Parser where
  empty = zero
  (<|>) = mplus

-- Produces the first character of the string as output
item :: Parser Char
item = Parser $ \case
  []       -> []
  (x : xs) -> [(x, xs)]

-- Succeeds if the first character matches a given one, otherwise fails
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then result x else zero

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return ""
string (x : xs) = [ a : as | a <- char x, as <- string xs ]

chain :: Parser a -> Parser b -> Parser (a, b)
chain p1 p2 = [ (a, b) | a <- p1, b <- p2 ]

zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = (return <$> p) <|> return []

many :: Parser a -> Parser [a]
many p = [ x : xs | x <- p, xs <- many p ] <|> return []

many1 :: Parser a -> Parser [a]
many1 p = [ x : xs | x <- p, xs <- many p ]

