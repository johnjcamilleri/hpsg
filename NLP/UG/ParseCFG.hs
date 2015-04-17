{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of Unification Grammars with a CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
module NLP.UG.ParseCFG where

import NLP.AVM
import NLP.UG.GrammarCFG

-- | Parse a string given a grammar and print output
--   Sets max tree depth based on length of input
parse :: String -> Grammar -> IO ()
parse s g = undefined
