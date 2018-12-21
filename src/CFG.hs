{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, LambdaCase, MonadComprehensions #-}

module CFG where

import Control.Monad.State.Lazy
import Control.Applicative ((<|>))
import Parser
import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)
import Debug.Trace
import Data.Map.Lazy (Map)

-- Newtype wrapper for type safety.
newtype CFG a = CFG { runCFG :: a }

-- A CFG in BCNF form.
class CFGSYM repr where
  -- Ts and nonterminals. The building blocks of expressions.
  t :: String -> repr
  n :: String -> repr

  -- Functions for building up an expression. 
  plus :: repr -> repr -- Zero or one of an expression
  star :: repr -> repr -- Zero or more of an expression
  (#) :: repr -> repr -> repr -- One expression after another 
  (|||) :: repr -> repr -> repr -- One expression or another

  -- A complete CFG. A list of (nonterminal, expression) pairs.
  -- Wrapped in a newtype to disallow further composition.
  rules :: [(String, repr)] -> CFG repr

infixl 4 #
infixl 3 |||

-- Pretty printer interpretation
instance CFGSYM String where
  t str = "\"" ++ str ++ "\""
  n str = str
  plus str = "[" ++ str ++ "]"
  star str = "{" ++ str ++ "}"
  str1 # str2 = str1 ++ " , " ++ str2
  str1 ||| str2 = str1 ++ " | " ++ str2

  rules = CFG . showRule where
    showRule [] = ""
    showRule ((n, s):rs) = n ++ " -> " ++ s ++ "\n" ++ showRule rs 

data ParseTree = 
  List [ParseTree]
  | N String [ParseTree]
  | T String
  deriving (Show)

toList :: ParseTree -> [ParseTree]
toList (T s) = [T s]
toList (List ts) = ts
toList (N _ ts) = ts

concatPT :: ParseTree ->  ParseTree -> ParseTree
concatPT a b = List (toList a ++ toList b)

instance CFGSYM (State (Map String (Parser ParseTree)) (Parser ParseTree)) where
  t str = return [T s | s <- string str]
    
  n str = do 
    parsers <- get 
    let p = fromJust $ Map.lookup str parsers
    return $ N str . toList <$> p

  plus parser = do
    p <- parser
    return [List xs | xs <- zeroOrOne p] 

  star parser = do
    p <- parser
    return [List xs | xs <- many p]

  parser1 # parser2 = do
    p1 <- parser1
    p2 <- parser2
    return [concatPT a b | a <- p1, b <- p2]
      
  parser1 ||| parser2 = do 
    p1 <- parser1 
    p2 <- parser2
    return $ p1 <|> p2

  -- TODO empty rule
  rules lst 
    | null lst = CFG $ return zero 
    | otherwise = CFG $ do
        let 
          allParsers :: Map String (Parser ParseTree)
          allParsers = foldr (\(name, parser) acc -> Map.insert name (evalState parser allParsers) acc) Map.empty lst
        put allParsers
        return $ fromJust $ Map.lookup (fst . head $ lst) allParsers

-- A test CFG. Represents arithmetic expressions on natural numbers.
arith = rules 
  [ 
    ("expr", 
      n "number" ||| t "(" # n "expr" # n "op" # n "expr" # t ")"),
    ("op", 
      t "+" ||| t "-" ||| t "*" ||| t "/"),
    ("number",
      t "0" ||| t "1" ||| t "2" ||| t "3" ||| t "4" ||| t "5" ||| t "6" ||| t "7" ||| t "8" ||| t "9")
  ]

ll1 = rules 
  [
    ("s", n "f"),
    ("s", t "(" # n "s" # t "+" # n "f" # t ")"),
    ("f", t "a")
  ]

simple = rules 
  [
    ("s", t "x" ||| t "x" # n "s")
  ]

arithParser :: (State (Map String (Parser ParseTree)) (Parser ParseTree))
arithParser = runCFG arith

arithP = evalState arithParser Map.empty

ll1Parser :: (State (Map String (Parser ParseTree)) (Parser ParseTree))
ll1Parser = runCFG ll1

ll1P = evalState ll1Parser Map.empty

simpleParser :: (State (Map String (Parser ParseTree)) (Parser ParseTree))
simpleParser = runCFG simple

simpleP = evalState simpleParser Map.empty