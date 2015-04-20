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
  rules :: [ERule]
  }
  deriving (Eq, Ord, Show)

-- | Category
type Cat = String

-- | Context-free rule
data Rule = Rule [Cat]          -- ^ Head is LHS, tail is RHS
          | Terminal Cat Token
  deriving (Eq, Ord, Show)

-- | Extended context-free rule
data ERule = ERule {
  rule :: Rule,
  mavm :: MultiAVM -- must have same length as rule
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
-- Helpers

-- | Is a tree complete?
--   Useful for filtering
isComplete :: DerivationTree -> Bool
isComplete (Leaf _) = True
isComplete (Node _ _ []) = False
isComplete (Node _ _ kids) = all isComplete kids

-- | Get linearisation from derivation tree
--   May include holes if tree is incomplete
lin :: DerivationTree -> [Token]
lin (Leaf tok) = [tok]
lin (Node _ _ []) = ["?"]
lin (Node _ _ kids) = concatMap lin kids

------------------------------------------------------------------------------
-- Pretty printing

pp :: DerivationTree -> IO ()
pp = putStrLn . ppTree

-- | Pretty print a derivation tree
ppTree :: DerivationTree -> String
ppTree = go 0
  where
    go :: Int -> DerivationTree -> String
    go l d = concat (replicate l "  ") ++ case d of
      Leaf tok -> show tok ++ "\n"
      Node cat avm kids -> cat ++ " " ++ inlineAVM avm ++ "\n" ++ concatMap (go (l+1)) kids

------------------------------------------------------------------------------
-- Examples

g1 :: Grammar
g1 = Grammar "s"
     [ ERule (Rule ["s", "np", "vp"]) (mkMultiAVM [vnullAVM,numX,numX])
     , ERule (Rule ["np", "d", "n"]) (mkMultiAVM [numX,numX,numX])
     , ERule (Rule ["vp", "v"]) (mkMultiAVM [numX,numX])
     , ERule (Rule ["vp", "v", "np"]) (mkMultiAVM [numX,numX,numY])

     -- Terminals
     , ERule (Terminal "n" "lamb") (mkMultiAVM [numX,numSG])
     , ERule (Terminal "n" "lambs") (mkMultiAVM [numX,numPL])
     , ERule (Terminal "n" "sheep") (mkMultiAVM [numX,numSG])
     , ERule (Terminal "n" "sheep") (mkMultiAVM [numX,numPL])

     , ERule (Terminal "v" "sleeps") (mkMultiAVM [numX,numSG])
     , ERule (Terminal "v" "sleep") (mkMultiAVM [numX,numPL])

     , ERule (Terminal "d" "a") (mkMultiAVM [numX,numSG])
     , ERule (Terminal "d" "two") (mkMultiAVM [numX,numPL])
     ]
  where
    numX = vmkAVM1 "NUM" (ValIndex 1)
    numY = vmkAVM1 "NUM" (ValIndex 2)
    numSG = vmkAVM [("NUM",ValIndex 1)] `vaddDict` [(1,ValAtom "sg")]
    numPL = vmkAVM [("NUM",ValIndex 1)] `vaddDict` [(1,ValAtom "pl")]

-- Adds case
g2 :: Grammar
g2 = Grammar "s"
     [ mkR ["s", "np", "vp"] [nullAVM,numX ⊔ casenom,numX]
     , mkR ["np", "d", "n"] [numX ⊔ caseY,numX,numX ⊔ caseY]
     , mkR ["np", "propn"] [numX ⊔ caseY,numX ⊔ caseY]
     , mkR ["np", "pron"]  [numX ⊔ caseY,numX ⊔ caseY]
     , mkR ["vp", "v", "np"] [numX,numX,numY ⊔ caseacc]

     -- Terminals
     , mkT "n" "lamb" numX numXsg
     -- , mkT "n" "lambs" numX numXpl
     -- , mkT "n" "sheep" numX numXsg
     -- , mkT "n" "sheep" numX numXpl
     , mkT "propn" "Rachel" (numX ⊔ caseY) (numXsg ⊔ caseY)
     -- , mkT "propn" "Jacob" (numX ⊔ caseY) (numXsg ⊔ caseY)

     , mkT "pron" "she" (numX ⊔ caseY) (numXsg ⊔ caseYnom)
     , mkT "pron" "her" (numX ⊔ caseY) (numXsg ⊔ caseYacc)

     , mkT "v" "herds" numX numXsg
     -- , mkT "v" "herd" numX numXpl

     , mkT "d" "a" numX numXsg
     -- , mkT "d" "two" numX numXpl
     ]
  where

    mkR :: [Cat] -> [AVM] -> ERule
    mkR cats avms
      | length cats /= length avms = error "mkR: unequal lengths"
      | otherwise = ERule (Rule cats) (mkMultiAVM (map ValAVM avms))

    mkT :: Cat -> Token -> AVM -> AVM -> ERule
    mkT cat tok avm1 avm2 = ERule (Terminal cat tok) (mkMultiAVM [ValAVM avm1, ValAVM avm2])

    numX = mkAVM1 "NUM" (ValIndex 1)
    numY = mkAVM1 "NUM" (ValIndex 2)
    numXsg = numX `addDict` [(1,ValAtom "sg")]
    numXpl = numX `addDict` [(1,ValAtom "pl")]
    casenom = mkAVM1 "CASE" (ValAtom "nom")
    caseacc = mkAVM1 "CASE" (ValAtom "acc")
    caseY = mkAVM1 "CASE" (ValIndex 2)
    caseYnom = caseY `addDict` [(2,ValAtom "nom")]
    caseYacc = caseY `addDict` [(2,ValAtom "acc")]
