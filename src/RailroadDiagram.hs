{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}

module RailroadDiagram where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

myCircle :: Diagram B
myCircle = circle 1

main = mainWith myCircle

