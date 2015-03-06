{-# LANGUAGE ScopedTypeVariables #-}
-- | "grammar1" from the ESSLLI 2000 HPSG Tutorial:
--    Ann Copestake, Dan Flickinger, Rob Malouf, Stephan Oepen
--    http://lingo.stanford.edu/esslli00/

import NLP.HPSG.AVM

import Data.Maybe (fromJust)

(&) a b = fromJust $ unify a b

av = ValAV . avmBody

_top_  = mkAVM []

_list_ = ValList []
string = mkAVM []

------------------------------------------------------------------------------
-- types

feat_struc :: AVM = _top_

syn_struc = feat_struc & mkAVM
  [ ("HEAD",av pos),
    ("SPR",_list_),
    ("COMPS",_list_)]

pos = feat_struc
noun = pos
verb = pos
det = pos

phrase = syn_struc & mkAVM
  [ ("ARGS",_list_) ]

word = syn_struc & mkAVM
  [ ("ORTH",av string) ]

root = phrase & mkAVM
  [ ("HEAD",ValAtom "verb"),
    ("SPR",ValList []),
    ("COMPS",ValList [])
  ]

------------------------------------------------------------------------------
-- rules

------------------------------------------------------------------------------
-- lexicon

the = word & mkAVM
  [ ("ORTH", ValAtom "the"),
    ("HEAD", av det),
    ("SPR",  ValList []),
    ("COMPS", ValList [])
  ]

that = word & mkAVM
  [ ("ORTH", ValAtom "that"),
    ("HEAD", av det),
    ("SPR",  ValList []),
    ("COMPS", ValList [])
  ]

those = word & mkAVM
  [ ("ORTH", ValAtom "those"),
    ("HEAD", av det),
    ("SPR",  ValList []),
    ("COMPS", ValList [])
  ]

dog = word & mkAVM
  [ ("ORTH", ValAtom "dog"),
    ("HEAD", av noun),
    ("SPR",  ValList [ ValAV $ mkAV [("HEAD",ValAtom "det")] ]),
    ("COMPS", ValList [])
  ]
