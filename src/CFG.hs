{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MonadComprehensions, NoMonomorphismRestriction #-}

module CFG where

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

-- A sample CFG. Represents fully parethesized arithmetic expressions on integers.
arith = rules
  [ ("expr", alt [n "integer", cat [t "(", n "expr", n "op", n "expr", t ")"]])
  , ("op"  , alt [t "+", t "-", t "*", t "/"])
  , ( "integer"
    , alt
      [t "0", cat [opt (t "-"), n "nonzero", rep (alt [t "0", n "nonzero"])]]
    )
  , ( "nonzero"
    , alt [t "1", t "2", t "3", t "4", t "5", t "6", t "7", t "8", t "9"]
    )
  ]
