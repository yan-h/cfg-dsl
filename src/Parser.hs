{-# LANGUAGE InstanceSigs, TypeSynonymInstances, FlexibleInstances, LambdaCase, MonadComprehensions #-}

module Parser where

import           Control.Monad.Reader
import           Data.Char
import           Data.Traversable
import           Control.Applicative            ( Alternative
                                                , empty
                                                , (<|>)
                                                )
import           Control.Monad
import Data.List(foldl')
import           CFG
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as Map

newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- Always succeeds, producing a constant result. Doesn't consume the input string.
result :: a -> Parser a
result v = Parser $ \cs -> [(v, cs)]

-- Always fails, producing no result.
zero :: Parser a
zero = Parser $ const []

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \cs -> [ (f a, res) | (a, res) <- parse p cs ]

instance Applicative Parser where
  pure :: a -> Parser a
  pure = result

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p <*> q =
    Parser $ \cs -> concat [ parse (f <$> q) res | (f, res) <- parse p cs ]

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = Parser $ \cs -> concat [ parse (f v) res | (v, res) <- parse p cs ]

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

-- Parses a single character and succeeds only if it fulfills a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
  x <- item
  if p x then result x else zero

-- Parses a single character
char :: Char -> Parser Char
char x = sat (== x)

-- Parses a string
string :: String -> Parser String
string []       = return ""
string (x : xs) = [ a : as | a <- char x, as <- string xs ]

-- Applies a parser zero or one times
zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = (return <$> p) <|> return []

-- Applies a parser zero or more times
many :: Parser a -> Parser [a]
many p = [ x : xs | x <- p, xs <- many p ] <|> return []

-- Applies a parser one or more times
many1 :: Parser a -> Parser [a]
many1 p = [ x : xs | x <- p, xs <- many p ]

-- Applies a parser and fails unless all of the input is consumed
complete :: Parser a -> Parser a
complete p = Parser $ \cs -> filter (\(_, res) -> res == "") (parse p cs)

-- Applies a parser and returns only its first result
one :: Parser a -> Parser a
one p = Parser $ \cs -> case parse p cs of
  []       -> []
  (x : xs) -> [x]

firstComplete :: Parser a -> Parser a
firstComplete = one . complete

catParsers :: [Parser a] -> Parser [a]
catParsers []       = result []
catParsers (p : ps) = do
  x <- p
  y <- catParsers ps
  return (x : y)

-- Parser interpretation
instance CFGSYM (Reader (Map String (Parser ParseTree)) (Parser ParseTree)) where
  t str = return [ T s | s <- string str ]

  n str = do
    parsers <- ask
    let p = case Map.lookup str parsers of
          (Just parser) -> parser
          Nothing -> error $ "Attempted to reference undeclared nonterminal: " ++ str
    return $ N str . toList <$> p

  opt parser = do
    p <- parser
    return [ List xs | xs <- zeroOrOne p ]

  rep parser = do
    p <- parser
    return [ List xs | xs <- many p ]

  cat parsers = do
    ps <- sequence parsers
    return [ catParseTrees pts | pts <- catParsers ps ]

  alt parsers = do
    ps <- sequence parsers
    return $ foldl' (<|>) zero ps

  rules lst
    | null lst
    = CFG $ return zero
    | otherwise
    = CFG $ 
      let startName = fst . head $ lst
          allParsers :: Map String (Parser ParseTree)
          allParsers = foldr
            (\(name, parser) acc ->
              Map.insert name (runReader parser allParsers) acc
            )
            Map.empty
            lst
          startParser = case Map.lookup startName allParsers of
            (Just parser) -> parser
            Nothing -> error "This should never happen"
      in  return [ N startName (toList xs) | xs <- complete startParser ]

-- The structure that our parsers produce.
data ParseTree =
  List [ParseTree] -- Temporary structure for accumulating results
  | N String [ParseTree] -- Nonterminal
  | T String -- Terminal

instance Show ParseTree where
  show (List xs  ) = show xs
  show (T    str ) = "\"" ++ str ++ "\""
  show (N name xs) = name ++ show xs

toList :: ParseTree -> [ParseTree]
toList (List ts) = ts
toList (T    s ) = [T s]
toList (N s ts ) = [N s ts]

catParseTrees :: [ParseTree] -> ParseTree
catParseTrees pts = List $ concatMap toList pts

getParser
  :: CFG (Reader (Map String (Parser ParseTree)) (Parser ParseTree))
  -> Parser ParseTree
getParser x = runReader (unCFG x) Map.empty

arithP :: Parser ParseTree
arithP = getParser arith
