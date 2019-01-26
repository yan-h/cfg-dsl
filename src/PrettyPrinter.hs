{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module PrettyPrinter where

import           Data.List                      ( intercalate )
import           CFG

-- Pretty printer interpretation
instance CFGSYM String where
  t str = "\"" ++ str ++ "\""
  n str = str
  opt str = "[" ++ str ++ "]"
  rep str = "{" ++ str ++ "}"
  cat   = intercalate " , "
  alt   = intercalate " | "

  rules = CFG . showRules   where
    showRules []            = ""
    showRules ((n, s) : rs) = n ++ " = " ++ s ++ ";\n" ++ showRules rs

arithStr :: String
arithStr = unCFG arith
