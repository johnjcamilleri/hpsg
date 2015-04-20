{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of Unification Grammars with no CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug3.pdf
module NLP.UG.Parse where

import Common

import NLP.AVM
import NLP.UG.Grammar

import qualified Data.Map as M

------------------------------------------------------------------------------
-- Naive parse function
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
              then combos [ [Node ravm []] | ravm <- rhs ]
              else combos [ go (d+1) ravm  | ravm <- rhs ]

        -- Get matching terminals
        f (Terminal tok lhs) =
          if avm ⊔? lhs
          then Just [ Node par [Leaf tok] ]
          else Nothing
          where
            par = lhs ⊔ avm -- new parent
            -- NOTE reindexing not necessary here as terminals have no indices

    -- Are all these kids compatible with parent?
    compatibleKids :: AVM -> [DerivationTree] -> Bool
    compatibleKids avm kids = fst $ foldl (\(t,b) (Node a _) -> (t && canMergeAVMDicts b a, mergeAVMDicts b a)) (True,avm) kids

    -- Merge these kids with parent (dictionaries)
    mergeKids :: AVM -> [DerivationTree] -> AVM
    mergeKids avm kids = foldl (\b (Node a _) -> mergeAVMDicts b a) avm kids

    -- Clean dictionaries of kids
    cleanKids :: [DerivationTree] -> [DerivationTree]
    cleanKids kids = [ Node (cleanDict avm) ks | Node avm ks <- kids ]
