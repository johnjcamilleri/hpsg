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
g1 = UG "s" [ Rule $ mkMultiAVM [cat "s" nullAVM, cat "np" numX, cat "vp" numX]
            , Rule $ mkMultiAVM [cat "np" numX, cat "d" numX, cat "N" numX]
            , Rule $ mkMultiAVM [cat "vp" numX, cat "v" numX]
            , Rule $ mkMultiAVM [cat "vp" numX, cat "v" numX, cat "vp" numY]

              -- Terminals
            , Rule $ mkMultiAVM [cat "n" numX, cat "lamb" numSG]
            , Rule $ mkMultiAVM [cat "n" numX, cat "sheep" numSG]

            , Rule $ mkMultiAVM [cat "v" numX, cat "sleeps" numSG]
            , Rule $ mkMultiAVM [cat "v" numX, cat "sleep" numPL]

            , Rule $ mkMultiAVM [cat "d" numX, cat "a" numSG]
            , Rule $ mkMultiAVM [cat "d" numX, cat "two" numPL]
            ]
     where
       cat c avm = ValAVM $ mkAVM1 "CAT" (ValAtom c) & avm
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       numSG = mkAVM [("NUM",ValIndex 1)] `addDict` [(1,ValAtom "sg")]
       numPL = mkAVM [("NUM",ValIndex 1)] `addDict` [(1,ValAtom "pl")]

g3 :: UG
g3 = UG "S" [ Rule $ mkMultiAVM [ cat "S" $ nullAVM
                                , cat "np" $ numX & (mkAVM1 "CASE" (ValAtom "nom"))
                                , cat "v" $ numX & (mkAVM1 "SUBCAT" (ValList []))
                                ]
            , Rule $ mkMultiAVM [ cat "v" $ numX & (mkAVM1 "SUBCAT" (ValIndex 2))
                                , cat "v" $ numX & (mkAVM1 "SUBCAT" (ValList [ValIndex 3, ValIndex 2]))
                                , ValIndex 3
                                ] `maddDict` [(3,vnullAVM)]
            , Rule $ mkMultiAVM [ cat "np" $ numX & caseY
                                , cat "d"  $ numX
                                , cat "n"  $ numX & caseY
                                ]
            , Rule $ mkMultiAVM [ cat "np" $ numX & caseY
                                , cat "pron" $ numX & caseY
                                ]
            , Rule $ mkMultiAVM [ cat "np" $ numX & caseY
                                , cat "propn" $ numX & caseY
                                ]

              -- TODO: how to represent terminals?
            , Rule $ mkMultiAVM [ ValAtom "sleep"
                                , cat "v" $ (mkAVM1 "SUBCAT" (ValList [])) & numPL
                                ]
            , Rule $ mkMultiAVM [ ValAtom "love"
                                , cat "v" $ (mkAVM1 "SUBCAT" (ValList [vmkAVM [("CAT",ValAtom "np"),("CASE",ValAtom "acc")]])) & numPL
                                ]
            , Rule $ mkMultiAVM [ ValAtom "give"
                                , cat "v" $ (mkAVM1 "SUBCAT" (ValList [vmkAVM [("CAT",ValAtom "np"),("CASE",ValAtom "acc")], vmkAVM1 "CAT" (ValAtom "np")])) & numPL
                                ]

            , Rule $ mkMultiAVM [ ValAtom "lamb",  cat "n" $ numSG & caseY ]
            , Rule $ mkMultiAVM [ ValAtom "lambs", cat "n" $ numPL & caseY ]
            , Rule $ mkMultiAVM [ ValAtom "she", cat "pron" $ numSG & mkAVM1 "CASE" (ValAtom "nom") ]
            , Rule $ mkMultiAVM [ ValAtom "her", cat "pron" $ numSG & mkAVM1 "CASE" (ValAtom "acc") ]
            , Rule $ mkMultiAVM [ ValAtom "Rachel", cat "propn" $ numSG ]
            , Rule $ mkMultiAVM [ ValAtom "Jacob", cat "propn" $ numSG ]
            , Rule $ mkMultiAVM [ ValAtom "a", cat "d" $ numSG ]
            , Rule $ mkMultiAVM [ ValAtom "two", cat "d" $ numPL ]

            ]
     where
       cat c avm = ValAVM $ mkAVM1 "CAT" (ValAtom c) & avm
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       numSG = mkAVM1 "NUM" (ValAtom "sg")
       numPL = mkAVM1 "NUM" (ValAtom "pl")
       caseY = mkAVM1 "CASE" (ValIndex 2)
