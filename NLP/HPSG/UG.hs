{-# LANGUAGE ScopedTypeVariables #-}

-- | Unification Grammars with no CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug3.pdf
module NLP.HPSG.UG where

import Common

import NLP.HPSG.AVM

import Data.Maybe (catMaybes)
import qualified Data.Map as M
import qualified Data.List as L

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
getCat avm = let Just (ValAtom c) = val [Attr "CAT"] avm in c

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
-- This essentially enumerates all trees to a certain depth and finds matching one

-- | Parse a string given a grammar and print output
--   Sets max tree depth based on length of input
parse :: String -> Grammar -> IO ()
parse s g = mapM_ pp $ parse' tokens g depth
  where
    tokens = words s
    depth = (\x -> ceiling $ 1 + log (5*fromIntegral x)) $ length tokens

-- | Parse a string given a grammar and print output
parseD :: String -> Grammar -> Int -> IO ()
parseD s g d = mapM_ pp $ parse' tokens g d
  where tokens = words s

-- | Parse a string given a grammar and return all possible derivation trees
parse' :: [Token] -> Grammar -> Int -> [DerivationTree]
parse' ts g d = [ tree | tree <- allTrees g d, lin tree == ts]

-- | Get all valid, complete sentences in a grammar, up to a certain tree depth
allStrings :: Grammar -> Int -> [String]
allStrings g depth = map (unwords.lin) $ filter isComplete $ allTrees g depth

-- | Get all valid derivation trees from a grammar, up to a certain depth
allTrees :: Grammar -> Int -> [DerivationTree]
allTrees g depth = allTrees' (mkAVM1 "CAT" (ValAtom $ start g)) g depth

allTrees' :: AVM -> Grammar -> Int -> [DerivationTree]
allTrees' avm g depth = go 0 avm
  where
    go :: Int -> AVM -> [DerivationTree]
    go d avm = concat drvs
      where
        drvs :: [[DerivationTree]] = filtermap f (rules g)
        f :: (Rule -> Maybe [DerivationTree])

        -- Get matching rules (recurses)
        f (Rule mavm) =
          if avm ⊔? lhs
          then Just [ Node (mergeKids par kids) (cleanKids kids) -- cleaning is probably unnecessary, just good practice
                    | kids <- kidss
                    , compatibleKids par kids ]
          else Nothing
          where
            -- replace all indices in rule to avoid clashes
            rs = M.fromList $ zip [1..100] [newIndex avm..] -- TODO remove this hard limit
            ravms = map (replaceIndices rs) (unMultiAVM mavm)
            lhs = head ravms
            rhs = map (setDict (avmDict par)) $ tail ravms
            par = lhs ⊔ avm -- new parent (want to keep indices as in LHS)
            kidss :: [[DerivationTree]] =
              if d >= depth-1
              then combos $ [ [Node ravm []] | ravm <- rhs ]
              else combos $ map (go (d+1)) rhs

        -- Get matching terminals
        f (Terminal tok lhs) =
          if avm ⊔? lhs
          then Just [ Node par [Leaf tok] ]
          else Nothing
          where
            par = lhs ⊔ avm -- new parent
            -- NOTE reindexing not necessary here is terminals have no indices

    -- Are all these kids compatible with parent?
    compatibleKids :: AVM -> [DerivationTree] -> Bool
    compatibleKids avm kids = fst $ foldl (\(t,b) (Node a _) -> (t && canMergeAVMDicts b a, mergeAVMDicts b a)) (True,avm) kids

    -- Merge these kids with parent (dictionaries)
    mergeKids :: AVM -> [DerivationTree] -> AVM
    mergeKids avm kids = foldl (\b (Node a _) -> mergeAVMDicts b a) avm kids

    -- Clean dictionaries of kids
    cleanKids :: [DerivationTree] -> [DerivationTree]
    cleanKids kids = [ Node (cleanDict avm) ks | Node avm ks <- kids ]

-- | Get linearisation from derivation tree
--   May include holes if tree is incomplete
lin :: DerivationTree -> [Token]
lin (Leaf tok) = [tok]
lin (Node _ []) = ["?"]
lin (Node _ kids) = concatMap lin kids

-- | Is a tree complete?
--   Usefule for filtering
isComplete :: DerivationTree -> Bool
isComplete (Leaf _) = True
isComplete (Node _ []) = False
isComplete (Node _ kids) = all isComplete kids

-- | Number of leaves in a tree
leafs :: DerivationTree -> Int
leafs (Leaf tok) = 1
leafs (Node _ []) = 0
leafs (Node _ kids) = sum (map leafs kids)

------------------------------------------------------------------------------
-- Examples

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

-- Adds case to g1
g2 :: Grammar
g2 = Grammar "s"
     [ Rule $ mkMultiAVM [ cat "s" nullAVM
                         , cat "np" $ numX ⊔ mkAVM1 "CASE" (ValAtom "nom")
                         , cat "vp" numX
                         ]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY
                         , cat "d" numX
                         , cat "n" $ numX ⊔ caseY
                         ]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY
                         , cat "pron" $ numX ⊔ caseY
                         ]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY
                         , cat "propn" $ numX ⊔ caseY
                         ]
     , Rule $ mkMultiAVM [ cat "vp" numX
                         , cat "v" $ numX ⊔ scIntrans
                         ]
     , Rule $ mkMultiAVM [ cat "vp" numX
                         , cat "v" $ numX ⊔ scTrans
                         , cat "np" $ numY ⊔ mkAVM1 "CASE" (ValAtom "acc")
                         ]

       -- Terminals
     , Terminal "sleeps" $ term "v" ⊔ numSG ⊔ scIntrans
     , Terminal "sleep" $ term "v" ⊔ numPL ⊔ scIntrans
     , Terminal "feeds" $ term "v" ⊔ numSG ⊔ scTrans
     , Terminal "feed" $ term "v" ⊔ numPL ⊔ scTrans

     , Terminal "lamb" $ term "n" ⊔ numSG ⊔ caseY
     , Terminal "lambs" $ term "n" ⊔ numPL ⊔ caseY
     , Terminal "she" $ term "pron" ⊔ numSG ⊔ mkAVM1 "CASE" (ValIndex 2) `addDict` [(2,ValAtom "nom")]
     , Terminal "her" $ term "pron" ⊔ numSG ⊔ mkAVM1 "CASE" (ValIndex 2) `addDict` [(2,ValAtom "acc")]
     , Terminal "John" $ term "propn" ⊔ numSG ⊔ caseY

     , Terminal "a" $ term "d" ⊔ numSG
     , Terminal "two" $ term "d" ⊔ numPL
     ]
     where
       cat c avm = ValAVM $ mkAVM1 "CAT" (ValAtom c) ⊔ avm
       term c = mkAVM1 "CAT" (ValAtom c)
       numX = mkAVM1 "NUM" (ValIndex 1)
       numY = mkAVM1 "NUM" (ValIndex 2)
       caseY = mkAVM1 "CASE" (ValIndex 2)
       scIntrans = mkAVM1 "SUBCAT" (ValAtom "intrans")
       scTrans = mkAVM1 "SUBCAT" (ValAtom "trans")
       numSG = mkAVM [("NUM",ValIndex 1)] `addDict` [(1,ValAtom "sg")]
       numPL = mkAVM [("NUM",ValIndex 1)] `addDict` [(1,ValAtom "pl")]

-- Subcategorisation lists, with number removed
g3a :: Grammar
g3a = Grammar "s"
     [ Rule $ mkMultiAVM [ ValAVM $ cat "s"
                         , ValAVM $ cat "np"
                         , ValAVM $ cat "v" ⊔ mkAVM1 "SUBCAT" (unlist [])
                         ]
     , Rule $ mkMultiAVM [ ValAVM $ cat "v" ⊔ mkAVM1 "SUBCAT" (ValIndex 2)
                         , ValAVM $ cat "v" ⊔ mkAVM1 "SUBCAT" (vmkAVM [ ("FIRST", vmkAVM1 "CAT" (ValIndex 1))
                                                                      , ("REST",  ValIndex 2)
                                                                      ])
                         , ValAVM $ mkAVM1 "CAT" (ValIndex 1)
                         ]

     , Terminal "John" $ term "np"
     , Terminal "Mary" $ term "np"
     -- , Terminal "Rachel" $ term "np"
     , Terminal "sleeps" $ term "v" ⊔ mkAVM1 "SUBCAT" (unlist [])
     , Terminal "loves"  $ term "v" ⊔ mkAVM1 "SUBCAT" (unlist [ vmkAVM1 "CAT" (ValAtom "np") ])
     , Terminal "gives"  $ term "v" ⊔ mkAVM1 "SUBCAT" (unlist [ vmkAVM1 "CAT" (ValAtom "np")
                                                              , vmkAVM1 "CAT" (ValAtom "np")
                                                              ])
     , Terminal "tells"  $ term "v" ⊔ mkAVM1 "SUBCAT" (unlist [ vmkAVM1 "CAT" (ValAtom "np")
                                                              , vmkAVM1 "CAT" (ValAtom "s")
                                                              ])
     ]
     where
       cat c = mkAVM1 "CAT" (ValAtom c)
       term c = mkAVM1 "CAT" (ValAtom c)

-- As in slides
g3 :: Grammar
g3 = Grammar "s"
     [ Rule $ mkMultiAVM [ cat "s" $ nullAVM
                         , cat "np" $ numX ⊔ (mkAVM1 "CASE" (ValAtom "nom"))
                         , cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" (unlist []))
                         ]
     , Rule $ mkMultiAVM [ cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" (ValIndex 2))
                         , cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" (vmkAVM [("FIRST",ValIndex 3), ("REST",ValIndex 2)]))
                         , ValIndex 3
                         ] `maddDict` [(3,vnullAVM)]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY
                         , cat "d"  $ numX
                         , cat "n"  $ numX ⊔ caseY
                         ]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY -- ⊔ queQ
                         , cat "pron" $ numX ⊔ caseY -- ⊔ queQ
                         ]
     , Rule $ mkMultiAVM [ cat "np" $ numX ⊔ caseY
                         , cat "propn" $ numX ⊔ caseY
                         ]

     , Terminal "sleep" $ term "v" ⊔ (mkAVM1 "SUBCAT" (unlist [])) ⊔ numPL
     , Terminal "love"  $ term "v" ⊔ (mkAVM1 "SUBCAT" (unlist [vmkAVM [("CAT",ValAtom "np"),("CASE",ValAtom "acc")]])) ⊔ numPL
     , Terminal "give"  $ term "v" ⊔ (mkAVM1 "SUBCAT" (unlist [vmkAVM [("CAT",ValAtom "np"),("CASE",ValAtom "acc")], vmkAVM1 "CAT" (ValAtom "np")])) ⊔ numPL

     , Terminal "lamb"  $ term "n" ⊔ numSG ⊔ caseY
     , Terminal "lambs" $ term "n" ⊔ numPL ⊔ caseY
     , Terminal "she" $ term "pron" ⊔ numSG ⊔ mkAVM1 "CASE" (ValAtom "nom")
     , Terminal "her" $ term "pron" ⊔ numSG ⊔ mkAVM1 "CASE" (ValAtom "acc")
     , Terminal "whom" $ term "pron" ⊔ mkAVM [("CASE",ValAtom "acc"){-,("QUE",ValAtom "+")-}]
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
