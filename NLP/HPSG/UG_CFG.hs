-- | Unification Grammars with a CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
module NLP.HPSG.UG_CFG where

import NLP.HPSG.AVM

data UG = UG {
  start :: ECat,
  rules :: [ECFRule]
  }

-- | Category
type Cat = String

-- | Extended category
type ECat = ECat String AVM

-- | Context-free rule
data CFRule = CFRule Cat [Cat]

-- | Extended context-free rule
data ECFRule = ECFRule {
  cfgRule :: CFRule,
  mavm :: MultiAVM -- must have same length
  }


------------------------------------------------------------------------------
-- Examples

g1 :: UG
g1 = UG "S" [ ECFRule (CFRule "S" ["NP", "VP"]) (mkMultiAVM [numX,numX])
            , ECFRule (CFRule "NP" ["D", "N"]) (mkMultiAVM [numX,numX,numX])
            , ECFRule (CFRule "VP" ["V"]) (mkMultiAVM [numX,numX])
            , ECFRule (CFRule "VP" ["V", "NP"]) (mkMultiAVM [numX,numX,numY])

              -- Terminals
            , ECFRule (CFRule "N" ["lamb"]) (mkMultiAVM [numX,numSG])
            , ECFRule (CFRule "N" ["sheep"]) (mkMultiAVM [numX,numSG])

            , ECFRule (CFRule "V" ["sleeps"]) (mkMultiAVM [numX,numSG])
            , ECFRule (CFRule "V" ["sleep"]) (mkMultiAVM [numX,numPl])

            , ECFRule (CFRule "D" ["a"]) (mkMultiAVM [numX,numSG])
            , ECFRule (CFRule "D" ["two"]) (mkMultiAVM [numX,numPL])
            ]
     where
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       numSG = mkAVM' [("NUM",ValIndex 1)] [(1,ValAtom "sg")]
       numPL = mkAVM' [("NUM",ValIndex 1)] [(1,ValAtom "pl")]
