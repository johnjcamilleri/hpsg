{-# LANGUAGE ScopedTypeVariables #-}

-- | Unification Grammars with a CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
module NLP.UG.GrammarCFG where

import Common
import NLP.AVM

------------------------------------------------------------------------------
-- Types

-- | Grammar
data Grammar = Grammar {
  start :: Cat,
  rules :: [Rule]
  }
  deriving (Eq, Ord, Show)

-- | Category
type Cat = String

-- | Context-free rule
data CFRule = CFRule [Cat]
            | Terminal Cat Token
  deriving (Eq, Ord, Show)

-- | Extended context-free rule
data Rule = Rule {
  cfRule :: CFRule,
  mavm :: MultiAVM -- must have same length as cfRule
  }
  deriving (Eq, Ord, Show)

-- | Token
type Token = String

-- | A derivation tree
--   Use only the dictionary in root AVM; all others should be empty
data DerivationTree = Leaf Token                    -- ^ A leaf
                    | Node Cat AVM [DerivationTree] -- ^ An intermediate node
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------
-- Examples

g1 :: Grammar
g1 = Grammar "s"
     [ Rule (CFRule ["s", "np", "vp"]) (mkMultiAVM [vnullAVM,numX,numX])
     , Rule (CFRule ["np", "d", "n"]) (mkMultiAVM [numX,numX,numX])
     , Rule (CFRule ["vp", "v"]) (mkMultiAVM [numX,numX])
     , Rule (CFRule ["vp", "v", "np"]) (mkMultiAVM [numX,numX,numY])

     -- Terminals
     , Rule (Terminal "n" "lamb") (mkMultiAVM [numX,numSG])
     , Rule (Terminal "n" "lambs") (mkMultiAVM [numX,numPL])
     , Rule (Terminal "n" "sheep") (mkMultiAVM [numX,numSG])
     , Rule (Terminal "n" "sheep") (mkMultiAVM [numX,numPL])

     , Rule (Terminal "v" "sleeps") (mkMultiAVM [numX,numSG])
     , Rule (Terminal "v" "sleep") (mkMultiAVM [numX,numPL])

     , Rule (Terminal "d" "a") (mkMultiAVM [numX,numSG])
     , Rule (Terminal "d" "two") (mkMultiAVM [numX,numPL])
     ]
  where
    numX = vmkAVM1 "NUM" (ValIndex 1)
    numY = vmkAVM1 "NUM" (ValIndex 2)
    numSG = vmkAVM [("NUM",ValIndex 1)] `vaddDict` [(1,ValAtom "sg")]
    numPL = vmkAVM [("NUM",ValIndex 1)] `vaddDict` [(1,ValAtom "pl")]
