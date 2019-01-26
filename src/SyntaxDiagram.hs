{-# LANGUAGE InstanceSigs, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, FlexibleInstances #-}

module SyntaxDiagram where

import           Diagrams.Prelude        hiding ( cat )
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.TwoD.Text
import           Data.List                      ( foldl'
                                                , foldl1'
                                                )
import           CFG

-- Height of a node box. Necessary to calculate arrow positioning
nodeHeight = 1.6
-- Gap between nodes when stacked on top of each other
vgap = 0.5
-- Gap between nodes horizontally connected with arrows
hpad = 1.4
lineThickness = 0.2

-- Common arrow style 
arrowStyle = with & arrowHead .~ dart & lengths .~ local 0.8

-- Shorthand for creating an arrow using the common style
mkArrow start end = arrowBetween' arrowStyle start end # lwL lineThickness

-- Attach incoming and outgoing arrows to a diagram
hArrows :: Double -> Diagram B -> Diagram B
hArrows maxWidth d =
  d # translateX hpad <> mkArrow (0 ^& 0) (hpad ^& 0) <> mkArrow
    ((width d + hpad) ^& 0)
    (maxWidth ^& 0)

-- Attach arrows connecting a diagram vertically to something below
vArrows :: Diagram B -> Diagram B
vArrows d =
  let h = height d
  in  d <> mkArrow (0 ^& 0) (0 ^& (-h - vgap)) <> mkArrow
        (width d ^& (-h - vgap))
        (width d ^& 0)

-- The awkward instance typing is because of the technical details of type families.
-- See https://stackoverflow.com/questions/45360959/illegal-type-synonym-family-application-in-instance-with-functional-dependency
instance (a ~ Diagram B) => CFGSYM a where
  -- A "node" on the diagram that represents a terminal. A rectangle enclosing some text.
  t :: String -> Diagram B
  t str =
    let
      txt       = text str # font "courier" # bold
      rectWidth = 0.6 + fromIntegral (length str) * 0.6
      rectangle =
        roundedRect rectWidth nodeHeight 0.5 # fc white # lwL lineThickness
    in
      (txt <> rectangle) # translateX (rectWidth * 0.5)

-- Same as the code for a terminal, with minor stylistic changes.
  n :: String -> Diagram B
  n str =
    let
      txt       = text str # font "courier"
      rectWidth = 0.6 + fromIntegral (length str) * 0.6
      rectangle = rect rectWidth nodeHeight # fc palegreen # lwL lineThickness
    in
      (txt <> rectangle) # translateX (rectWidth * 0.5)

-- Connects a chain of diagrams using horizontal arrows.
  cat :: [Diagram B] -> Diagram B
  cat ds
    | null ds
    = mempty
    | otherwise
    = let combine :: Diagram B -> Diagram B -> Diagram B
          combine chain new =
            localize
              $ (chain # named "chain" ||| strut hpad ||| new # named "new")
              # connectPerim' arrowStyle
                              "chain"
                              "new"
                              (0 @@ turn)
                              (1 / 2 @@ turn)
              # lwL lineThickness
      in  foldl1' combine ds

-- Makes a vertical "stack" of diagrams, connected on the left and right with arrows.
  alt :: [Diagram B] -> Diagram B
  alt ds
    | null ds
    = mempty
    | otherwise
    = let hlen        = maximum (map width ds) + hpad * 2
          withHArrows = map (hArrows hlen) ds
          withVArrows = map vArrows (init withHArrows) ++ [last withHArrows]
      in  vsep (-0.5 * nodeHeight) withVArrows -- Stack everything vertically

-- Adds horizontal arrows to a diagram, and a parallel line that "skips" it.
  opt :: Diagram B -> Diagram B
  opt d =
    let hlen   = width d + hpad * 2
        top    = mkArrow (0 ^& 0) (hlen ^& 0)
        bottom = hArrows hlen d
        height = nodeHeight * 0.5 + vgap
    in  vsep vgap [top, bottom] <> mkArrow (0 ^& 0) (0 ^& (-height)) <> mkArrow
          (hlen ^& (-height))
          (hlen ^& 0) # extrudeEnvelope (0 ^& (-height - nodeHeight * 0.25))

-- Adds horizontal arrows to a diagram, and a loop from its end to its beginning.
  rep :: Diagram B -> Diagram B
  rep d =
    let hlen   = width d + hpad * 2
        top    = hArrows hlen d
        bottom = mkArrow (hlen ^& 0) (0 ^& 0)
    in  vsep vgap [top, bottom]
        <> mkArrow (0 ^& (-(height top + vgap - nodeHeight * 0.5))) (0 ^& 0)
        <> mkArrow (hlen ^& 0)
                   (hlen ^& (-(height top + vgap - nodeHeight * 0.5)))

-- Displays all diagrams in a vertical stack, and labels each by its nonterminal name.
  rules :: [(String, Diagram B)] -> CFG (Diagram B)
  rules xs =
    CFG
      $ let drawProd :: (String, Diagram B) -> Diagram B
            drawProd (name, d) =
              let txt =
                    text (name ++ ":")
                      # fontSize (local 1.2)
                      # translateX (fromIntegral (length name) * 0.4)
                      # font "courier"
                      # bold
              in  vsep (vgap * 1.5) [txt, hArrows (width d + hpad * 2) d]
        in  frame 2 $ (vsep (vgap * 4) . map drawProd $ xs)

arithDiagram :: Diagram B
arithDiagram = unCFG arith

-- Grammars for showing off each function in the DSL

nDemo :: Diagram B
nDemo = n "nt"

tDemo :: Diagram B
tDemo = t "t"

catDemo :: Diagram B
catDemo = cat [n "nt", t "t", n "nt"]

altDemo :: Diagram B
altDemo = alt [n "nt", t "t", n "nt"]

optDemo :: Diagram B
optDemo = opt (n "nt")

repDemo :: Diagram B
repDemo = rep (t "t")

allDemos :: Diagram B
allDemos = frame 1 $ vsep (2 * vgap) [nDemo, tDemo, catDemo, altDemo, optDemo, repDemo]