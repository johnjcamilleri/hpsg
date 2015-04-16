-- | Unification Grammars with a CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
module NLP.UG.GrammarCFG where

import NLP.AVM

data Grammar = Grammar {
  start :: Cat,
  rules :: [ECFRule]
  }

-- | Category
type Cat = String

-- | Context-free rule
data CFRule = CFRule Cat [Cat]

-- | Extended context-free rule
data ECFRule = ECFRule {
  cfgRule :: CFRule,
  mavm :: MultiAVM -- must have same length
  }


------------------------------------------------------------------------------
-- Examples

g1 :: Grammar
g1 = Grammar "S"
     [ ECFRule (CFRule "S" ["NP", "VP"]) (mkMultiAVM [numX,numX])
     , ECFRule (CFRule "NP" ["D", "N"]) (mkMultiAVM [numX,numX,numX])
     , ECFRule (CFRule "VP" ["V"]) (mkMultiAVM [numX,numX])
     , ECFRule (CFRule "VP" ["V", "NP"]) (mkMultiAVM [numX,numX,numY])

     -- Terminals
     , ECFRule (CFRule "N" ["lamb"]) (mkMultiAVM [numX,numSG])
     , ECFRule (CFRule "N" ["sheep"]) (mkMultiAVM [numX,numSG])

     , ECFRule (CFRule "V" ["sleeps"]) (mkMultiAVM [numX,numSG])
     , ECFRule (CFRule "V" ["sleep"]) (mkMultiAVM [numX,numPL])

     , ECFRule (CFRule "D" ["a"]) (mkMultiAVM [numX,numSG])
     , ECFRule (CFRule "D" ["two"]) (mkMultiAVM [numX,numPL])
     ]
  where
    numX = vmkAVM1 "NUM" (ValIndex 1)
    numY = vmkAVM1 "NUM" (ValIndex 2)
    numSG = vmkAVM [("NUM",ValIndex 1)] `vaddDict` [(1,ValAtom "sg")]
    numPL = vmkAVM [("NUM",ValIndex 1)] `vaddDict` [(1,ValAtom "pl")]
