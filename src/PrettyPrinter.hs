{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, InstanceSigs, MonadComprehensions, NoMonomorphismRestriction #-}
module PrettyPrinter where

import           Data.List                      ( intercalate )
import CFG
  
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
    showRule ((n, s) : rs) = n ++ " = " ++ s ++ ";\n" ++ showRule rs

arithStr :: String
arithStr = unCFG arith