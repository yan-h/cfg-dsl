{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MonadComprehensions, NoMonomorphismRestriction #-}

module CFG where

import           Control.Applicative            ( (<|>) )
import           Debug.Trace
import           Data.List                      ( intercalate )

newtype CFG a = CFG {unCFG :: a}

-- A CFG in EBNF form.
class CFGSYM repr where
  -- Terminals and nonterminals. The building blocks of expressions.
  t :: String -> repr
  n :: String -> repr

  -- Functions for building up an expression. 
  cat :: [repr] -> repr -- Concatenation: Chaining a list of expressions. 
  alt :: [repr] -> repr -- Alternation: Choosing one of a list of expressions.
  opt :: repr -> repr -- Optional: zero or one of an expression.
  rep :: repr -> repr -- Repetition: zero or more of an expression.

  -- A complete CFG. A list of (nonterminal, expression) pairs.
  -- Wrapped in a newtype to disallow further composition.
  rules :: [(String, repr)] -> CFG repr

-- Pretty printer interpretation
instance CFGSYM String where
  t str = "\"" ++ str ++ "\""
  n str = str
  opt str = "[" ++ str ++ "]"
  rep str = "{" ++ str ++ "}"
  cat   = intercalate " , "
  alt   = intercalate " | "

  rules = CFG . showRule where
    showRule []            = ""
    showRule ((n, s) : rs) = n ++ " -> " ++ s ++ "\n" ++ showRule rs

-- A test CFG. Represents arithmetic expressions on natural numbers.
arith = rules
  [ ("expr", alt [n "number", cat [t "(", n "expr", n "op", n "expr", t ")"]])
  , ("op"  , alt [t "+", t "-", t "*", t "/"])
  , ( "number"
    , alt
      [ t "0"
      , cat
        [ opt (t "-")
        , n "nonzero"
        , alt
          [ rep (alt [t "0", n "nonzero", rep (alt [t "a", t "b", t "c"])])
          , cat [t "a", t "b", t "c", t "d", t "e", t "f"]
          ]
        ]
      ]
    )
  , ( "nonzero"
    , alt [t "1", t "2", t "3", t "4", t "5", t "6", t "7", t "8", t "9"]
    )
  ]

simple = rules [("s", alt [t "x", cat [t "x", n "s"]])]

arithStr :: String
arithStr = unCFG arith

simpleStr :: String
simpleStr = unCFG simple

