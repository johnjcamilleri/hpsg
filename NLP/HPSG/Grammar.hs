module NLP.HPSG.Grammar where

import NLP.HPSG.AVM

data Grammar = Grammar {
  rules :: [AVM],
  types :: ()
  }
