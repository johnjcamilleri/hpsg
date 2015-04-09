{-# LANGUAGE ScopedTypeVariables #-}

-- | Unification Grammars with no CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug3.pdf
module NLP.HPSG.UG where

import Common

import NLP.HPSG.AVM
import Debug.Trace (trace)

data Grammar = Grammar {
  start :: Cat,
  rules :: [Rule]
  }
  deriving (Eq, Ord, Show)

type Token = String

-- | Category
type Cat = String

-- | Rule is essentially a multi-avm, where first item is LHS
data Rule = Rule MultiAVM
          | Terminal Token AVM
  deriving (Eq, Ord, Show)

-- | A derivation tree
-- Hack: use only the dictionary in root AVM
data DerivationTree = Leaf Token
                    | Node AVM [DerivationTree]
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------
-- Helpers

getCat :: AVM -> Cat
getCat avm = let Just (ValAtom c) = val avm [Attr "CAT"] in c

------------------------------------------------------------------------------
-- Pretty printing

pp :: DerivationTree -> IO ()
pp = putStrLn . ppTree

ppTree :: DerivationTree -> String
ppTree = go 0
  where
    go :: Int -> DerivationTree -> String
    go l d = concat (replicate l "  ") ++ case d of
      Leaf tok -> show tok ++ "\n"
      Node avm kids -> inlineAVM avm ++ "\n" ++ concatMap (go (l+1)) kids

------------------------------------------------------------------------------
-- Actual parse function

-- | Stupid, brute-force parsing
parse :: Grammar -> String -> IO ()
parse g s = mapM_ pp $ parse' g (words s)

-- | Stupid, brute-force parsing
parse' :: Grammar -> [Token] -> [DerivationTree]
parse' g ts = [ tree | tree <- allTrees g, lin tree == ts]

-- | Enumerate all valid derivation trees from a grammar
--   May not terminate
allTrees :: Grammar -> [DerivationTree]
allTrees g = go (mkAVM1 "CAT" (ValAtom $ start g))
  where
    go :: AVM -> [DerivationTree]
    go avm = concat rs2 ++ ts2 -- these two lists should be disjoint
      where
        -- Get matching rules (recurses)
        rs2 :: [[DerivationTree]] = filtermap fr2 (rules g)
        fr2 :: (Rule -> Maybe [DerivationTree])
        fr2 (Rule mavm) =
          if toAVM mavm 1 ⊔? avm
          then Just [ Node (unifyKids par kids) kids | kids <- kidss, unifiableKids par kids ]
          else Nothing
          where
            par = toAVM mavm 1 ⊔ avm -- new parent
            kidss :: [[DerivationTree]] = combos $ map go (tail $ unMultiAVM mavm)
        fr2 _ = Nothing

        -- Get matching terminals
        ts2 :: [DerivationTree] = filtermap ft2 (rules g)
        ft2 :: (Rule -> Maybe DerivationTree)
        ft2 (Terminal tok avm') =
          if avm ⊔? avm'
          then Just $ Node (avm ⊔ avm') [Leaf tok]
          else Nothing
        ft2 _ = Nothing

    -- Are all these kids unifiable with parent?
    unifiableKids :: AVM -> [DerivationTree] -> Bool
    unifiableKids avm kids = fst $ foldl (\(t,b) (Node a _) -> (t && b ⊔-? a, b ⊔- a)) (True,avm) kids

    -- Unify all these kids with parent
    unifyKids :: AVM -> [DerivationTree] -> AVM
    unifyKids avm kids = foldl (\b (Node a _) -> b ⊔- a) avm kids

    -- Unification, ignoring CAT category (retaining first)
    (⊔-) :: AVM -> AVM -> AVM
    (⊔-) a b = addAttr a'' (a' ⊔ b')
      where
        (a',a'') = remAttr (Attr "CAT") a
        (b',_  ) = remAttr (Attr "CAT") b

    -- Unifiable ignoring CAT category
    (⊔-?) :: AVM -> AVM -> Bool
    (⊔-?) a b = a' ⊔? b'
      where
        (a',_) = remAttr (Attr "CAT") a
        (b',_) = remAttr (Attr "CAT") b

-- | Get linearisation from derivation tree
--   May include holes if tree is incomplete
lin :: DerivationTree -> [Token]
lin (Leaf tok) = [tok]
lin (Node _ []) = ["?"]
lin (Node _ kids) = concatMap lin kids

------------------------------------------------------------------------------
-- Examples

tr :: DerivationTree
tr = Node (mkAVM [("CAT",ValAtom "s")] `addDict` [(1,ValAtom "pl")])
     [ Node (mkAVM [("CAT",ValAtom "np"),("NUM",ValIndex 1)])
       [ Node (mkAVM [("CAT",ValAtom "d"),("NUM",ValIndex 1)]) [Leaf "two"]
       , Node (mkAVM [("CAT",ValAtom "n"),("NUM",ValIndex 1)]) [Leaf "sheep"]
       ]
     , Node (mkAVM [("CAT",ValAtom "vp"),("NUM",ValIndex 1)])
       [ Node (mkAVM [("CAT",ValAtom "v"),("NUM",ValIndex 1)]) [Leaf "sleep"]
       ]
     ]

g1 :: Grammar
g1 = Grammar "s"
     [ Rule $ mkMultiAVM [ cat "s" nullAVM
                         , cat "np" numX
                         , cat "vp" numX
                         ]
     , Rule $ mkMultiAVM [ cat "np" numX
                         , cat "d" numX
                         , cat "n" numX
                         ]
     , Rule $ mkMultiAVM [ cat "vp" numX
                         , cat "v" numX
                         ]
     , Rule $ mkMultiAVM [ cat "vp" numX
                         , cat "v" numX
                         , cat "np" numY
                         ]

       -- Terminals
     , Terminal "lamb" $ term "n" ⊔ numSG
     , Terminal "lambs" $ term "n" ⊔ numPL
     , Terminal "sheep" $ term "n" ⊔ numSG
     , Terminal "sheep" $ term "n" ⊔ numPL

     , Terminal "sleeps" $ term "v" ⊔ numSG
     , Terminal "sleep" $ term "v" ⊔ numPL

     , Terminal "a" $ term "d" ⊔ numSG
     , Terminal "two" $ term "d" ⊔ numPL
     ]
     where
       cat c avm = ValAVM $ mkAVM1 "CAT" (ValAtom c) ⊔ avm
       term c = mkAVM1 "CAT" (ValAtom c)
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       numSG = mkAVM [("NUM",ValIndex 1)] `addDict` [(1,ValAtom "sg")]
       numPL = mkAVM [("NUM",ValIndex 1)] `addDict` [(1,ValAtom "pl")]

-- Adds case
g3 :: Grammar
g3 = Grammar "s"
     [ Rule $ mkMultiAVM [ cat "s" $ nullAVM
                         , cat "np" $ numX ⊔ (mkAVM1 "CASE" (ValAtom "nom"))
                         , cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" (ValList []))
                         ]
     , Rule $ mkMultiAVM [ cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" (ValIndex 2))
                         , cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" (ValList [ValIndex 3, ValIndex 2]))
                         , ValIndex 3
                         ] `maddDict` [(3,vnullAVM)]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY
                         , cat "d"  $ numX
                         , cat "n"  $ numX ⊔ caseY
                         ]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY ⊔ queQ
                         , cat "pron" $ numX ⊔ caseY ⊔ queQ
                         ]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY
                         , cat "propn" $ numX ⊔ caseY
                         ]

     , Terminal "sleep" $ term "v" ⊔ (mkAVM1 "SUBCAT" (ValList [])) ⊔ numPL
     , Terminal "love"  $ term "v" ⊔ (mkAVM1 "SUBCAT" (ValList [vmkAVM [("CAT",ValAtom "np"),("CASE",ValAtom "acc")]])) ⊔ numPL
     , Terminal "give"  $ term "v" ⊔ (mkAVM1 "SUBCAT" (ValList [vmkAVM [("CAT",ValAtom "np"),("CASE",ValAtom "acc")], vmkAVM1 "CAT" (ValAtom "np")])) ⊔ numPL

     , Terminal "lamb"  $ term "n" ⊔ numSG ⊔ caseY
     , Terminal "lambs" $ term "n" ⊔ numPL ⊔ caseY
     , Terminal "she" $ term "pron" ⊔ numSG ⊔ mkAVM1 "CASE" (ValAtom "nom")
     , Terminal "her" $ term "pron" ⊔ numSG ⊔ mkAVM1 "CASE" (ValAtom "acc")
     , Terminal "whom" $ term "pron" ⊔ mkAVM [("CASE",ValAtom "acc"),("QUE",ValAtom "+")]
     , Terminal "Rachel" $ term "propn" ⊔ numSG
     , Terminal "Jacob"  $ term "propn" ⊔ numSG
     , Terminal "a"   $ term "d" ⊔ numSG
     , Terminal "two" $ term "d" ⊔ numPL

     ]
     where
       cat c avm = ValAVM $ mkAVM1 "CAT" (ValAtom c) ⊔ avm
       term c = mkAVM1 "CAT" (ValAtom c)
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       numSG = mkAVM1 "NUM" (ValAtom "sg")
       numPL = mkAVM1 "NUM" (ValAtom "pl")
       caseY = mkAVM1 "CASE" (ValIndex 2)
       queQ = mkAVM1 "QUE" (ValIndex 4)
