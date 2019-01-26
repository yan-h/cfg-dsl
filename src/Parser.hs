{-# LANGUAGE InstanceSigs, TypeSynonymInstances, FlexibleInstances, LambdaCase, MonadComprehensions #-}

module Parser where

import           Control.Monad.Reader
import           Data.Char
import           Data.Traversable
import           Control.Applicative            ( Alternative
                                                , empty
                                                , (<|>)
                                                )
import           Data.Maybe                     ( fromJust )
import           Control.Monad
import           Data.List                      ( foldl' )
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

-- Takes two parsers and produces a parser that applies them both and concatenates the resulting lists.
plus :: Parser a -> Parser a -> Parser a
plus p p' = Parser $ \cs -> parse p cs ++ parse p' cs

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
  mzero = zero
  mplus = plus

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
  if p x then result x else empty

-- Parses a single character
char :: Char -> Parser Char
char x = sat (== x)

-- Parses a string
string :: String -> Parser String
string []       = return ""
string (x : xs) = [ a : as | a <- char x, as <- string xs ]

-- Applies a parser zero or one times
zeroOrOne :: Parser a -> Parser [a]
zeroOrOne p = (return <$> p) `plus` return []

-- Applies a parser zero or more times
many :: Parser a -> Parser [a]
many p = [ x : xs | x <- p, xs <- many p ] `plus` return []

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

-- Applies a parser and returns the first result that completely consumes the input
firstComplete :: Parser a -> Parser a
firstComplete = one . complete

-- Turns a list of parsers into a parser that produces a list, in the obvious way
catParsers :: [Parser a] -> Parser [a]
catParsers []       = result []
catParsers (p : ps) = do
  x <- p
  y <- catParsers ps
  return (x : y)

-- Parser interpretation
instance CFGSYM (Reader (Map String (Parser ParseTree)) (Parser ParseTree)) where
  -- For a terminal, return a parser that accepts precisely a string
  t str = return [ T s | s <- string str ]

  -- For a nonterminal, use its definition in the map of all parsers. Laziness makes
  -- the cyclic dependency OK
  n str = do
    parsers <- ask
    let p = case Map.lookup str parsers of
          (Just parser) -> parser
          Nothing ->
            error $ "Attempted to reference undeclared nonterminal: " ++ str
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
    where 
      catParseTrees :: [ParseTree] -> ParseTree
      catParseTrees pts = List $ concatMap toList pts

  alt parsers = do
    ps <- sequence parsers
    return $ foldr plus zero ps

  rules lst
    | null lst
    = CFG $ return zero
    | otherwise
    = CFG
      $ let startName = fst . head $ lst
            -- A map of all the parsers in the list. The parser of each production rule 
            -- that contains nonterminals is dependent on this map, 
            -- so we store it in the Reader monad
            allParsers :: Map String (Parser ParseTree)
            allParsers = foldr
              (\(name, parser) acc ->
                Map.insert name (runReader parser allParsers) acc
              )
              Map.empty
              lst
            startParser = fromJust $ Map.lookup startName allParsers
            -- The result is the parser for the first nonterminal in the list. The parser
            -- should fail unless it completely parses the input. 
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

-- Convert an arbitrary ParseTree to a list of ParseTree. Used to label 
-- parse results with a nonterminal name.
toList :: ParseTree -> [ParseTree]
toList (List ts) = ts
toList (T    s ) = [T s]
toList (N s ts ) = [N s ts]

-- Unwrap a parser produced from this DSL
getParser
  :: CFG (Reader (Map String (Parser ParseTree)) (Parser ParseTree))
  -> Parser ParseTree
getParser x = runReader (unCFG x) Map.empty

arithParser :: Parser ParseTree
arithParser = getParser arith

nonTerminating = rules 
  [
    ("x", alt [t "1", cat [n "x", t "2"]])
  ]

nonTerminatingParser :: Parser ParseTree
nonTerminatingParser = getParser nonTerminating
