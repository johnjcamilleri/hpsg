-- | Unification Grammars with no CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug3.pdf
module NLP.HPSG.UG where

import NLP.HPSG.AVM

data UG = UG {
  start :: Cat,
  rules :: [Rule]
  }

-- | Category
type Cat = String

-- | Rule is essentially a multi-avm, where first item is LHS
data Rule = Rule MultiAVM

------------------------------------------------------------------------------
-- Examples

g1 :: UG
g1 = UG "S" [ Rule $ mkMultiAVM [cat "S" nullAVM, cat "NP" numX, cat "VP" numX]
            , Rule $ mkMultiAVM [cat "NP" numX, cat "D" numX, cat "N" numX]
            , Rule $ mkMultiAVM [cat "VP" numX, cat "V" numX]
            , Rule $ mkMultiAVM [cat "VP" numX, cat "V" numX, cat "VP" numY]

              -- Terminals
            , Rule $ mkMultiAVM [cat "N" numX, cat "lamb" numSG]
            , Rule $ mkMultiAVM [cat "N" numX, cat "sheep" numSG]

            , Rule $ mkMultiAVM [cat "V" numX, cat "sleeps" numSG]
            , Rule $ mkMultiAVM [cat "V" numX, cat "sleep" numPL]

            , Rule $ mkMultiAVM [cat "D" numX, cat "a" numSG]
            , Rule $ mkMultiAVM [cat "D" numX, cat "two" numPL]
            ]
     where
       cat c avm = mkAVM1 "CAT" (ValAtom c) & avm
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       numSG = mkAVM' [("NUM",ValIndex 1)] [(1,ValAtom "sg")]
       numPL = mkAVM' [("NUM",ValIndex 1)] [(1,ValAtom "pl")]

g3 :: UG
g3 = UG "S" [ Rule $ mkMultiAVM [ cat "S" $ nullAVM
                                , cat "NP" $ numX & (mkAVM1 "CASE" (ValAtom "nom"))
                                , cat "V" $ numX & (mkAVM1 "SUBCAT" (ValList []))
                                ]
            , Rule $ mkMultiAVM [ cat "V" $ numX & (mkAVM1 "SUBCAT" (ValIndex 2))
                                , cat "V" $ numX & (mkAVM1 "SUBCAT" (ValList [ValIndex 1, ValIndex 2]))
                                , mkAVM' "SUBCAT" (ValList [ValIndex 1, ValIndex 2])) --- ARG
                                ]
            , Rule $ mkMultiAVM [ cat "NP" $ numX & (mkAVM1 "CASE" (ValIndex 2))
                                , cat "D" $ numX
                                , cat "N" $ numX & (mkAVM1 "CASE" (ValIndex 2))
                                ]
            ]
     where
       cat c avm = mkAVM1 "CAT" (ValAtom c) & avm
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       numSG = mkAVM' [("NUM",ValIndex 1)] [(1,ValAtom "sg")]
       numPL = mkAVM' [("NUM",ValIndex 1)] [(1,ValAtom "pl")]
