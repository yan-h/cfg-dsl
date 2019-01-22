# A Tagless-Final DSL for Context-Free Grammars

This is a class project. The aim was to create a language for defining the rules of CFGs, and to allow multiple interpretations for terms in the language by using [tagless final style](http://okmij.org/ftp/tagless-final/course/lecture.pdf).

One interpretation of the language is as parsers using [monadic parser combinators](http://www.cs.nott.ac.uk/~pszgmh/monparsing.pdf). 

The other interpretation is SVG railroad diagrams using the [Diagrams](http://hackage.haskell.org/package/diagrams) library.
