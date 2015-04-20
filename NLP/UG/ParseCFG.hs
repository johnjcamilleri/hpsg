{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of Unification Grammars with a CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
module NLP.UG.ParseCFG where

import Common
import NLP.AVM
import NLP.UG.GrammarCFG

import qualified Data.Map as M
import Debug.Trace (trace)

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
allTrees g depth = allTrees' (start g) nullAVM g depth

allTrees' :: Cat -> AVM -> Grammar -> Int -> [DerivationTree]
allTrees' cat avm g depth = go 0 cat avm
  where
    go :: Int -> Cat -> AVM -> [DerivationTree]
    go d cat avm = concat drvs
      where
        drvs :: [[DerivationTree]] = filtermap f (rules g)
        f :: (ERule -> Maybe [DerivationTree])

        -- Get matching rules (recurses)
        f (ERule (Rule (clhs:crhs)) mavm) =
          if cat == clhs && avm ⊔? lhs
          then Just [ Node clhs (mergeKids par kids) (cleanKids kids) -- cleaning is probably unnecessary, just good practice
                    | kids <- kidss -- kids :: [DerivationTree]
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
              then combos $ [ [Node c ravm []] | (c,ravm) <- zip crhs rhs ]
              else combos $ [ go (d+1) c ravm | (c,ravm) <- zip crhs rhs ]

        -- Get matching terminals
        f (ERule (Terminal clhs tok) mavm) =
          if cat == clhs -- && avm ⊔? lhs
          then Just [ Node clhs par [Leaf tok] ]
          else Nothing
          where
            rs = M.fromList $ zip [1..100] [newIndex avm..] -- TODO remove this hard limit
            ravms = map (replaceIndices rs) (unMultiAVM mavm)
            lhs = head ravms
            -- [rhs] = map (setDict (avmDict par)) $ tail ravms
            par = lhs ⊔ avm -- new parent
            -- NOTE reindexing not necessary here as terminals have no indices

    -- Are all these kids compatible with parent?
    compatibleKids :: AVM -> [DerivationTree] -> Bool
    compatibleKids avm kids = fst $ foldl (\(t,b) (Node _ a _) -> (t && canMergeAVMDicts b a, mergeAVMDicts b a)) (True,avm) kids

    -- Merge these kids with parent (dictionaries)
    mergeKids :: AVM -> [DerivationTree] -> AVM
    mergeKids avm kids = foldl (\b (Node _ a _) -> mergeAVMDicts b a) avm kids

    -- Clean dictionaries of kids
    cleanKids :: [DerivationTree] -> [DerivationTree]
    cleanKids kids = [ Node c (cleanDict avm) ks | Node c avm ks <- kids ]
