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

-- | Stupid, brute-force parsing
parse :: String -> Grammar -> IO ()
parse s g = mapM_ pp $ parse' tokens g (length tokens * 2) -- how much depth?
  where tokens = words s

parseD :: String -> Grammar -> Int -> IO ()
parseD s g d = mapM_ pp $ parse' tokens g d
  where tokens = words s

parse' :: [Token] -> Grammar -> Int -> [DerivationTree]
parse' ts g d = [ tree | tree <- allTrees g d, lin tree == ts]

-- | Enumerate all valid derivation trees from a grammar, up to a certain depth
allTrees :: Grammar -> Int -> [DerivationTree]
allTrees g depth = go 0 (mkAVM1 "CAT" (ValAtom $ start g))

-- TEMP
-- allTrees g depth = go 0 (mkAVM [("CAT",ValAtom "v"),("SUBCAT",vnullAVM)])
-- allTrees g depth = go 0 (mkAVM [("CAT",ValAtom "v"),("SUBCAT", unlist [vmkAVM1 "CAT" (ValIndex 1), vmkAVM1 "CAT" (ValIndex 1)])])
-- allTrees g depth = go 0 (mkAVM [("CAT",ValAtom "v"),("SUBCAT",(vmkAVM [("FIRST", vmkAVM1 "CAT" (ValIndex 1)), ("REST",ValIndex 2)]))])
  where
    go :: Int -> AVM -> [DerivationTree]
    go d avm | d >= depth = []
    go d avm = concat drvs
      where
        drvs :: [[DerivationTree]] = filtermap f (rules g)
        f :: (Rule -> Maybe [DerivationTree])

        -- Get matching rules (recurses)
        f (Rule mavm) =
          if avm ⊔? lhs
          then Just [ Node (unifyKids par kids) kids | kids <- kidss, unifiableKids par kids ]
          else Nothing
          where
            lhs = toAVM mavm 1
            rhs = tail $ unMultiAVM mavm
            par = lhs ⊔ avm -- new parent
            kidss :: [[DerivationTree]] = combos $ map (go (d+1)) rhs

        -- Get matching terminals
        f (Terminal tok lhs) =
          if avm ⊔? lhs
          then Just [ Node (avm ⊔ lhs) [Leaf tok] ]
          else Nothing

    -- TODO: Still unsure if we want full unification here or just dict merging

    -- Are all these kids unifiable with parent?
    unifiableKids :: AVM -> [DerivationTree] -> Bool
    unifiableKids avm kids = fst $ foldl (\(t,b) (Node a _) -> (t && b ⊔-? a, b ⊔- a)) (True,avm) kids
    -- unifiableKids avm kids = fst $ foldl (\(t,b) (Node a _) -> (t && canMergeAVMDicts b a, mergeAVMDicts b a)) (True,avm) kids

    -- Unify all these kids with parent
    unifyKids :: AVM -> [DerivationTree] -> AVM
    unifyKids avm kids = foldl (\b (Node a _) -> b ⊔- a) avm kids
    -- unifyKids avm kids = foldl (\b (Node a _) -> mergeAVMDicts b a) avm kids

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

-- | Number of leaves in a tree
leafs :: DerivationTree -> Int
leafs (Leaf tok) = 1
leafs (Node _ []) = 0
leafs (Node _ kids) = sum (map leafs kids)

------------------------------------------------------------------------------
-- Examples

-- tr :: DerivationTree
-- tr = Node (mkAVM [("CAT",ValAtom "s")] `addDict` [(1,ValAtom "pl")])
--      [ Node (mkAVM [("CAT",ValAtom "np"),("NUM",ValIndex 1)])
--        [ Node (mkAVM [("CAT",ValAtom "d"),("NUM",ValIndex 1)]) [Leaf "two"]
--        , Node (mkAVM [("CAT",ValAtom "n"),("NUM",ValIndex 1)]) [Leaf "sheep"]
--        ]
--      , Node (mkAVM [("CAT",ValAtom "vp"),("NUM",ValIndex 1)])
--        [ Node (mkAVM [("CAT",ValAtom "v"),("NUM",ValIndex 1)]) [Leaf "sleep"]
--        ]
--      ]

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

-- Subcategorisation lists
g2 :: Grammar
g2 = Grammar "s"
     [ Rule $ mkMultiAVM [ ValAVM $ cat "s"
                         , ValAVM $ cat "np"
                         -- , ValAVM $ cat "v" ⊔ mkAVM1 "SUBCAT" (unlist [])
                         , ValAVM $ cat "v" ⊔ mkAVM1 "SUBCAT" (vnullAVM)
                         ]
     , Rule $ mkMultiAVM [ ValAVM $ cat "v" ⊔ mkAVM1 "SUBCAT" (ValIndex 2)
                         , ValAVM $ cat "v" ⊔ mkAVM1 "SUBCAT" (vmkAVM [("FIRST", vmkAVM1 "CAT" (ValIndex 1)), ("REST",ValIndex 2)])
                         , ValAVM $ mkAVM1 "CAT" (ValIndex 1)
                         ]

     , Terminal "John" $ term "np"
     , Terminal "Mary" $ term "np"
     , Terminal "Rachel" $ term "np"
     , Terminal "sleeps" $ term "v" ⊔ subcats 0
     , Terminal "loves"  $ term "v" ⊔ subcats 1
     , Terminal "gives"  $ term "v" ⊔ subcats 2

     ]
     where
       subcats n = mkAVM1 "SUBCAT" (unlist (replicate n (vmkAVM1 "CAT" (ValAtom "np"))))
       cat c = mkAVM1 "CAT" (ValAtom c)
       term c = mkAVM1 "CAT" (ValAtom c)

-- test :: IO ()
-- test = do
--   let
--     avm = cat "s"

--     cat c = mkAVM1 "CAT" (ValAtom c)
--     term c = mkAVM1 "CAT" (ValAtom c)
--     rules = [ Rule $ mkMultiAVM [ ValAVM $ avm10
--                                 , ValAVM $ avm11
--                                 , ValAVM $ avm12
--                                 ]
--             , Rule $ mkMultiAVM [ ValAVM $ avm20
--                                 , ValAVM $ avm21
--                                 , ValAVM $ avm22
--                                 ]

--             , Terminal "Rachel" $ term "np"
--             , Terminal "the-sheep" $ term "np"
--             , Terminal "some-hay" $ term "np"
--             , Terminal "gave" $ term "v"
--             ]
--     avm10 = cat "s"
--     avm11 = cat "np"
--     avm12 = cat "v" ⊔ mkAVM1 "SUBCAT" (unlist [])

--     avm20 = cat "v" ⊔ mkAVM1 "SUBCAT" (ValIndex 2)
--     avm21 = cat "v" ⊔ mkAVM1 "SUBCAT" (vmkAVM [("FIRST", vmkAVM1 "CAT" (ValIndex 1)), ("REST",ValIndex 2)])
--     avm22 = mkAVM1 "CAT" (ValIndex 1)

--     par = avm12 ⊔ avm20

--   print $ par ⊔? avm21
--   -- putStrLn $ ppAVM $ avm12 ⊔ avm20

-- Adds case
g3 :: Grammar
g3 = Grammar "s"
     [ Rule $ mkMultiAVM [ cat "s" $ nullAVM
                         , cat "np" $ numX ⊔ (mkAVM1 "CASE" (ValAtom "nom"))
                         -- , cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" (unlist []))
                         , cat "v" $ numX ⊔ (mkAVM1 "SUBCAT" vnullAVM)
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
