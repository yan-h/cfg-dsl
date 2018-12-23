{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MonadComprehensions, NoMonomorphismRestriction #-}

module CFG where

import           Control.Monad.Reader
import           Control.Applicative            ( (<|>) )
import           Parser
import           Data.Maybe                     ( fromJust )
import           Debug.Trace
import           Data.List                      ( intercalate )
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as Map

-- A CFG in EBNF form.
class CFGSYM repr where
  -- Terminals and nonterminals. The building blocks of expressions.
  t :: String -> repr
  n :: String -> repr

  -- Functions for building up an expression. 
  cat :: [repr] -> repr -- Concatnation: Chaining a list of expressions. 
  alt :: [repr] -> repr -- Alternation: Choosing one of a list of expressions.
  opt :: repr -> repr -- Optional: zero or one of an expression.
  rep :: repr -> repr -- Repetition: zero or more of an expression.

  -- A complete CFG. A list of (nonterminal, expression) pairs.
  -- Wrapped in a newtype to disallow further composition.
  rules :: [(String, repr)] -> repr

-- Pretty printer interpretation
instance CFGSYM String where
  t str = "\"" ++ str ++ "\""
  n str = str
  opt str = "[" ++ str ++ "]"
  rep str = "{" ++ str ++ "}"
  cat = intercalate " , "
  alt = intercalate " | "

  rules = showRule where
    showRule [] = ""
    showRule ((n, s):rs) = n ++ " -> " ++ s ++ "\n" ++ showRule rs

-- Parser interpretation
instance CFGSYM (Reader (Map String (Parser ParseTree)) (Parser ParseTree)) where
  t str = return [T s | s <- string str]

  n str = do
    parsers <- ask
    let p = fromJust $ Map.lookup str parsers
    return $ N str . toList <$> p

  opt parser = do
    p <- parser
    return [List xs | xs <- zeroOrOne p]

  rep parser = do
    p <- parser
    return [List xs | xs <- many p]

  cat parsers = do
    ps <- sequence parsers
    return [catParseTrees pts | pts <- catParsers ps]

  alt parsers = do
    ps <- sequence parsers
    return $ foldr (<|>) zero ps

  rules lst
    | null lst = return zero
    | otherwise =
        let startName = fst . head $ lst
            allParsers :: Map String (Parser ParseTree)
            allParsers = foldr (\(name, parser) acc -> Map.insert name (runReader parser allParsers) acc) Map.empty lst
            startParser = fromJust $ Map.lookup startName allParsers
        in return [N startName (toList xs) | xs <- complete startParser]

-- The structure that our parsers produce.
data ParseTree =
  List [ParseTree] -- Temporary structure for accumulating results
  | N String [ParseTree] -- Nonterminal
  | T String -- Terminal

instance Show ParseTree where
  show (List xs) = show xs
  show (T str) = str
  show (N name xs) = name ++ show xs

toList :: ParseTree -> [ParseTree]
toList (List ts) = ts
toList (T    s ) = [T s]
toList (N s ts ) = [N s ts]

catParseTrees :: [ParseTree] -> ParseTree
catParseTrees pts = List $ concatMap toList pts

getParser
  :: Reader (Map String (Parser ParseTree)) (Parser ParseTree)
  -> Parser ParseTree
getParser x = runReader x Map.empty

-- A test CFG. Represents arithmetic expressions on natural numbers.
arith = rules
  [ ("expr", alt [n "number", cat [t "(", n "expr", n "op", n "expr", t ")"]])
  , ("op"    , alt [t "+", t "-", t "*", t "/"])
  , ("number", alt [t "0", cat [opt (t "-"), n "nonzero", alt [rep (alt [t "0", n "nonzero", rep (alt [t "a", t "b", t "c"])]), cat [t "a", t "b", t "c", t "d", t "e", t "f"]]]])
  , ("nonzero"
    , alt [t "1", t "2", t "3", t "4", t "5", t "6", t "7", t "8", t "9"]
    )
  ]

simple = rules [("s", alt [t "x", cat [t "x", n "s"]])]

arithStr :: String
arithStr = arith

simpleStr :: String
simpleStr = simple

arithP :: Parser ParseTree
arithP = getParser arith

simpleP :: Parser ParseTree
simpleP = getParser simple
