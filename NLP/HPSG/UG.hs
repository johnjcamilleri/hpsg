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
          if avm ⊔? lhs -- OK
          then Just [ Node (mergeKids par kids) (cleanKids rs kids) | kids <- kidss, compatibleKids par kids ]
          else Nothing
          where
            lhs = toAVM mavm 1
            rhs = tail $ unMultiAVM mavm
            par = avm ⊔ lhs -- new parent
            rs = repls lhs par -- index replacements from above unification
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
            par = avm ⊔ lhs -- new parent

    -- Are all these kids unifiable with parent?
    -- unifiableKids :: AVM -> [DerivationTree] -> Bool
    -- unifiableKids avm kids = fst $ foldl (\(t,b) (Node a _) -> (t && b ⊔-? a, b ⊔- a)) (True,avm) kids
    -- unifiableKids avm kids = all (\(Node a _) -> (avm ⊔-? a)) kids

    -- -- Unify all these kids with parent
    -- unifyKids :: AVM -> [DerivationTree] -> AVM
    -- unifyKids avm kids = foldl (\b (Node a _) -> b ⊔- a) avm kids

    -- Are all these kids compatible with parent?
    compatibleKids :: AVM -> [DerivationTree] -> Bool
    compatibleKids avm kids = fst $ foldl (\(t,b) (Node a _) -> (t && canMergeAVMDicts b a, mergeAVMDicts b a)) (True,avm) kids

    -- Merge these kids with parent (dictionaries)
    mergeKids :: AVM -> [DerivationTree] -> AVM
    mergeKids avm kids = foldl (\b (Node a _) -> mergeAVMDicts b a) avm kids

    -- Clean dictionaries of kids
    cleanKids :: M.Map Index Index -> [DerivationTree] -> [DerivationTree]
    cleanKids rs kids = [ Node (replaceIndices rs $ cleanDict avm) ks | Node avm ks <- kids ]

    -- -- Unification, ignoring CAT category (retaining first)
    -- (⊔-) :: AVM -> AVM -> AVM
    -- (⊔-) a b = addAttr a'' (a' ⊔ b')
    --   where
    --     (a',a'') = remAttr (Attr "CAT") a
    --     (b',_  ) = remAttr (Attr "CAT") b

    -- -- Unifiable ignoring CAT category
    -- (⊔-?) :: AVM -> AVM -> Bool
    -- (⊔-?) a b = a' ⊔? b'
    --   where
    --     (a',_) = remAttr (Attr "CAT") a
    --     (b',_) = remAttr (Attr "CAT") b

-- | Given two AVMs, try to figure out which indices where renamed
--   This would be best generated during unification, but i'm hoping this is a quick fix
--   Note the "direction" of the arguments
repls :: AVM -> AVM -> M.Map Index Index
repls a1 a2 = M.fromList $ L.nub $ catMaybes $ map f (paths a1)
  where
    f :: Path -> Maybe (Index, Index)
    f p = case (val p a1, val p a2) of
      (Just (ValIndex i1), Just (ValIndex i2)) -> if i1 /= i2 then Just (i1,i2) else Nothing
      _ -> Nothing
    -- Our own version of val which doesn't follow indices
    val :: Path -> AVM -> Maybe Value
    val [] avm = Nothing
    val [a] avm = val' a avm
    val (a:as) avm = case val' a avm of
      Just (ValAVM avm') -> val as avm'
      _ -> Nothing
    val' a avm = M.lookup a (avmBody avm)

-- test = do
--   let
--     avm1 = mkAVM [("CAT",ValAtom "np"),("NUM",ValIndex 3)] `addDict` [(3,ValAtom "pl")]
--     avm2 = mkAVM [("NUM",ValIndex 2)]
--     u = avm1 ⊔ avm2
--   putStrLn $ inlineAVM avm2
--   putStrLn $ inlineAVM u
--   print $ repls avm2 u

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
     -- , Rule $ mkMultiAVM [ cat "vp" numX
     --                     , cat "v" numX
     --                     ]
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
     -- , Terminal "Mary" $ term "np"
     -- , Terminal "Rachel" $ term "np"
     -- , Terminal "sleeps" $ term "v" ⊔ subcats 0
     , Terminal "loves"  $ term "v" ⊔ subcats 1
     , Terminal "gives"  $ term "v" ⊔ subcats 2
     -- , Terminal "tells"  $ term "v" ⊔ mkAVM1 "SUBCAT" (unlist [vmkAVM1 "CAT" (ValAtom "np"), vmkAVM1 "CAT" (ValAtom "s")])
     ]
     where
       subcats n = mkAVM1 "SUBCAT" (unlist (replicate n (vmkAVM1 "CAT" (ValAtom "np"))))
       cat c = mkAVM1 "CAT" (ValAtom c)
       term c = mkAVM1 "CAT" (ValAtom c)

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
