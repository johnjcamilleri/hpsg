{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Parsing of Unification Grammars with a CFG backbone
--   Following http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
module NLP.UG.ParseCFG where

import Common
import NLP.AVM
import NLP.UG.GrammarCFG

import qualified Data.Char as C
import qualified Data.Map as M
import qualified Data.List as L
import qualified Control.Monad.State as CMS
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
allTrees' cat avm g depth =
  CMS.evalState (go 0 [1] cat avm) (newIndex avm)
  where
    go :: (CMS.MonadState Int m) => Int -> [Int] -> Cat -> AVM -> m [DerivationTree]
    go d pth cat avm = do
      drvs :: [[DerivationTree]] <- filtermapM f (rules g)
      return $ concat drvs
      where
        f :: (CMS.MonadState Int m) => ERule -> m (Maybe [DerivationTree])

        -- Get matching rules (recurses)
        f (ERule (Rule (clhs:crhs)) mavm) = do
          let
            from :: Int = newIndex avm -- read (concat (map show (reverse pth)))
            rs = M.fromList $ zip [1..100] [from..] -- TODO remove this hard limit
            ravms = unMultiAVM $ mreIndex rs mavm
            lhs = head ravms
            rhs = map (setDict (avmDict par)) $ tail ravms
            par = lhs ⊔ avm
          kidss' :: [[DerivationTree]] <-
            if d < depth-1
            then sequence [ go (d+1) (i:pth) c ravm  | (i,(c,ravm)) <- zip [1..] (zip crhs rhs) ]
            else return   [ [Node c ravm []] | (c,ravm) <- zip crhs rhs ]
          let kidss = combos kidss'

          if cat == clhs && avm ⊔? lhs
          then return $ Just [ Node clhs (mergeKids par kids) (cleanKids kids) -- cleaning is probably unnecessary, just good practice
                             | kids <- kidss -- kids :: [DerivationTree]
                             , compatibleKids par kids ]
          else return $ Nothing

        -- Get matching terminals
        f (ERule (Terminal clhs tok) mavm) = do
          let
            from :: Int = newIndex avm -- read (concat (map show (reverse pth)))
            rs = M.fromList $ zip [1..100] [from..] -- TODO remove this hard limit
            lhs:rhs:[] = unMultiAVM $ mreIndex rs mavm
            par = rhs ⊔ avm
          if cat == clhs && avm ⊔? rhs
          then return $ Just [ Node clhs par [Leaf tok] ]
          else return $ Nothing

    -- Are all these kids compatible with parent?
    compatibleKids :: AVM -> [DerivationTree] -> Bool
    compatibleKids avm kids = fst $ foldl (\(t,b) (Node _ a _) -> (t && canMergeAVMDicts b a, mergeAVMDicts b a)) (True,avm) kids
    -- compatibleKids avm kids = fst $ foldl (\(t,b) (Node _ a _) -> (t && b ⊔? a, b ⊔ a)) (True,avm) kids

    -- Merge these kids with parent (dictionaries)
    mergeKids :: AVM -> [DerivationTree] -> AVM
    mergeKids avm kids = foldl (\b (Node _ a _) -> mergeAVMDicts b a) avm kids
    -- mergeKids avm kids = foldl (\b (Node _ a _) -> b ⊔ a) avm kids

    -- Clean dictionaries of kids
    cleanKids :: [DerivationTree] -> [DerivationTree]
    cleanKids kids = [ Node c (cleanDict avm) ks | Node c avm ks <- kids ]

------------------------------------------------------------------------------
-- Test

test :: IO ()
test = do
  let
    ss1 = allStrings g1 6
    ss1' = ["a lamb sleeps"
           ,"a lamb sleeps a lamb"
           ,"a lamb sleeps a sheep"
           ,"a lamb sleeps two lambs"
           ,"a lamb sleeps two sheep"
           ,"a sheep sleeps"
           ,"a sheep sleeps a lamb"
           ,"a sheep sleeps a sheep"
           ,"a sheep sleeps two lambs"
           ,"a sheep sleeps two sheep"
           ,"two lambs sleep"
           ,"two lambs sleep a lamb"
           ,"two lambs sleep a sheep"
           ,"two lambs sleep two lambs"
           ,"two lambs sleep two sheep"
           ,"two sheep sleep"
           ,"two sheep sleep a lamb"
           ,"two sheep sleep a sheep"
           ,"two sheep sleep two lambs"
           ,"two sheep sleep two sheep"]

    ss2 = allStrings g2 6
    ss2' = ["a lamb herds a lamb"
           ,"a lamb herds Rachel"
           ,"a lamb herds her"
           ,"Rachel herds a lamb"
           ,"Rachel herds Rachel"
           ,"Rachel herds her"
           ,"she herds a lamb"
           ,"she herds Rachel"
           ,"she herds her"]

  f ss1' ss1
  f ss2' ss2

  where
    f gold out = do
      if L.sort gold == L.sort out
      then
        putStrLn "Ok"
      else do
        let under = gold L.\\ out
            over  = out L.\\ gold
        putStrLn "Under:"
        putStrLn $ unlines $ under
        putStrLn "Over:"
        putStrLn $ unlines $ over
        putStrLn $ show (length under) ++ "/" ++ show (length over) ++ "/" ++ show (length gold)
      putStrLn "------------------------"
