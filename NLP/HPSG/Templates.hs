module NLP.HPSG.Templates where

import NLP.HPSG.AVM

------------------------------------------------------------------------------
-- Templates

np_i :: AV
np_i = mkAV1 "LOC" $ vmkAV [("CAT", vmkAV [("HEAD", ValAtom "noun"), ("SUBCAT", ValList [])]),
                            ("CONTENT", vmkAV1 "INDEX" (ValIndex 0))]

s_i :: AV
s_i = mkAV1 "LOC" $ vmkAV [("CAT", vmkAV [("HEAD", ValAtom "verb"), ("SUBCAT", ValList [])]),
                           ("CONTENT", ValIndex 0)]

vp_i :: AV
vp_i = mkAV1 "LOC" $ vmkAV [("CAT", vmkAV [("HEAD", ValAtom "verb"), ("SUBCAT", ValList [ValAtom "synsem"])]),
                            ("CONTENT", ValIndex 0)]

------------------------------------------------------------------------------
-- Lexicon entries?

-- PS94 pg 28
walks_V :: AV
walks_V = mkAV [("CAT", vmkAV [("HEAD",ValAtom "verb[fin]"),("SUBCAT",ValList [])]),
                ("CONTENT", vmkAV [("RELN", ValAtom "walk"),("WALKER", ValIndex 1)])]
