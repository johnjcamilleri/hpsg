{-# LANGUAGE ScopedTypeVariables #-}

module NLP.AVM.Test where

import qualified Data.Map as M
import Data.Maybe (fromJust)

import Test.QuickCheck
import qualified Control.Monad.Writer as CMW

import NLP.AVM

------------------------------------------------------------------------------
-- Properties
-- Taken from http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf

prop_equality_commutative :: AVM -> AVM -> Property
prop_equality_commutative a b =
  a ~= b ==> b ~= a

--

prop_subsumption_least :: AVM -> Bool
prop_subsumption_least a = nullAVM ⊑ a

prop_subsumption_reflexive :: AVM -> Bool
prop_subsumption_reflexive a = a ⊑ a

prop_subsumption_transitive :: AVM -> AVM -> AVM -> Property
prop_subsumption_transitive a b c =
  (a ⊑ b) && (b ⊑ c) ==> a ⊑ c

prop_subsumption_antisymmetric :: AVM -> AVM -> Property
prop_subsumption_antisymmetric a b =
  (a ⊑ b) && (b ⊑ a) ==> a ~= b

--

prop_subsumes_implies_unifiable :: AVM -> AVM -> Property
prop_subsumes_implies_unifiable a b =
  a ⊑ b ==> a ⊔? b

--

prop_unification_idempotent :: AVM -> Bool
prop_unification_idempotent a =
  (a ⊔? a) && (a ⊔ a ~= a)

prop_unification_commutative :: AVM -> AVM -> Property
prop_unification_commutative a b =
  (a ⊔? b) && (distinctDicts a b) ==> (b ⊔? a) && (a ⊔ b ~= b ⊔ a)

prop_unification_associative :: AVM -> AVM -> AVM -> Property
prop_unification_associative a b c =
  and
    [ (a ⊔? b)
    , (b ⊔? c)
    , (a ⊔? (b ⊔ c))
    , ((a ⊔ b) ⊔? c)
    , distinctDicts a b
    , distinctDicts a c
    , distinctDicts b c
    ] ==> a ⊔ (b ⊔ c) ~= (a ⊔ b) ⊔ c

prop_unification_absorbing :: AVM -> AVM -> Property
prop_unification_absorbing a b =
  a ⊑ b ==> a ⊔ b ~= b

prop_unification_monotonic :: AVM -> AVM -> AVM -> Property
prop_unification_monotonic a b c =
  and
    [ a ⊑ b
    , a ⊔? c
    , b ⊔? c
    ] ==> (a ⊔ c) ⊑ (b ⊔ c)

prop_unification_most_general :: AVM -> AVM -> Property
prop_unification_most_general b c =
  b ⊔? c ==> let a = b ⊔ c in (b ⊑ a) && (c ⊑ a)

--

prop_generalisation_idempotent :: AVM -> Bool
prop_generalisation_idempotent a = (a ⊓ a ~= a)

prop_generalisation_commutative :: AVM -> AVM -> Property
prop_generalisation_commutative a b =
  (distinctDicts a b) ==> (a ⊓ b ~= b ⊓ a)

prop_generalisation_absorbing :: AVM -> AVM -> Property
prop_generalisation_absorbing a b =
  a ⊑ b ==> a ⊓ b ~= a

props = do
  let args = stdArgs
        { maxSize = 10          -- default = 100
        , chatty = True         -- default = True
        , maxSuccess = 100      -- default = 100
        , maxDiscardRatio = 100 -- default = 10
        }

  -- putStrLn "[ Equality ]"
  -- putStrLn "Commutativity" >> quickCheckWith args prop_equality_commutative
  -- putStrLn ""
  -- putStrLn "[ Subsumption ]"
  -- putStrLn "Least element" >> quickCheckWith args prop_subsumption_least -- ok
  -- putStrLn "Reflexivity" >> quickCheckWith args prop_subsumption_reflexive -- ok
  -- putStrLn "Transitivity" >> quickCheckWith args prop_subsumption_transitive -- ok
  -- putStrLn "Anti-symmetry" >> quickCheckWith args prop_subsumption_antisymmetric -- ok
  -- putStrLn "Implies unifiable" >> quickCheckWith args prop_subsumes_implies_unifiable -- ok
  -- putStrLn ""
  putStrLn "[ Unification ]"
  -- putStrLn "Idempotency" >> quickCheckWith args prop_unification_idempotent -- ok
  -- putStrLn "Commutativity" >> quickCheckWith args prop_unification_commutative -- ok
  -- putStrLn "Associativity" >> quickCheckWith args prop_unification_associative -- ok
  -- putStrLn "Absorption" >> quickCheckWith args prop_unification_absorbing -- ok
  putStrLn "Monotinicity" >> quickCheckWith args prop_unification_monotonic -- not ok
  putStrLn "Most general" >> quickCheckWith args prop_unification_most_general -- not ok
  -- putStrLn ""
  -- putStrLn "[ Generalisation ]"
  -- putStrLn "Idempotency" >> quickCheckWith args prop_generalisation_idempotent -- ok
  -- putStrLn "Commutativity" >> quickCheckWith args prop_generalisation_commutative -- ok
  -- putStrLn "Absorption" >> quickCheckWith args prop_generalisation_absorbing -- ok

------------------------------------------------------------------------------
-- Counter-examples

pp s avm = do
  putStrLn $ "--- " ++ s ++ " ---"
  putStrLn $ ppAVM avm

-- cx_anti_symmetry = do
--   let a = mkAVM [("C",ValNull)]
--   let b = mkAVM [("C",vnullAVM)]
--   -- (a ⊑ b) && (b ⊑ a) ==> a ~= b
--   pp "a" a
--   pp "b" b
--   assert $ a ⊑ b
--   assert $ b ⊑ a
--   assert $ a ~= b

-- cx_implies_unifiable = do
--   let a = mkAVM' [("B",ValIndex 1)] [(1,ValList [])]
--   let b = mkAVM  [("B",vmkAVM [("A",ValList [])] )]
--   -- a ⊑ b ==> a ⊔? b
--   pp "a" a
--   pp "b" b
--   assert $ a ⊑ b
--   assert $ a ⊔? b

-- cx_idempotency = do
--   let a = mkAVM' [("B",ValIndex 2)] [(2,ValAtom "z")]
--   pp "a" a
--   pp "a ⊔ a" $ a ⊔ a
--   assert $ a ~= a ⊔ a

-- cx_commutativity = do
--   let a = mkAVM' [("C",ValIndex 5)] [(5,ValList [])]
--   let b = mkAVM' [("C",ValIndex 3)] [(3,ValList [ValList [ValNull]])]
--   pp "a" a
--   pp "b" b
--   pp "a ⊔ b" $ a ⊔ b
--   pp "b ⊔ a" $ b ⊔ a
--   assert $ a ⊔ b ~= b ⊔ a

-- cx_absorption = do
--   let a = mkAVM' [("C",ValIndex 1)] [(1,ValNull)]
--   let b = mkAVM' [("B",ValList [vmkAVM [("B",ValIndex 1)]]),("C",ValNull)] [(1,ValAtom "x")]
--   pp "a" a
--   pp "b" b
--   pp "a ⊔ b" $ a ⊔ b
--   assert $ a ⊑ b
--   assert $ a ⊔ b ~= b

-- cx_monotonic = do
--   -- a ⊑ b ==> (a ⊔ c) ⊑ (b ⊔ c)
--   let a = nullAVM
--   let b = mkAVM [("B",ValList [])]
--   let c = mkAVM [("B",vnullAVM)]
--   pp "a" a
--   pp "b" b
--   pp "c" c
--   assert $ a ⊑ b
--   assert $ a ⊔? c
--   assert $ b ⊔? c
--   pp "a ⊔ c" $ a ⊔ c
--   pp "b ⊔ c" $ b ⊔ c
--   assert $ (a ⊔ c) ⊑ (b ⊔ c)

-- cx_most_general = do
--   -- b ⊔? c ==> let a = b ⊔ c in (b ⊑ a) && (c ⊑ a)
--   let b = mkAVM [("A",vnullAVM)]
--   let c = mkAVM' [("A",ValIndex 4)] [(4,ValList [])]

--   -- TODO: This is problematic cos of cycles (best to just avoid)
--   -- let b = mkAVM [("A",ValNull),("C",ValAtom "x")]
--   -- let c = mkAVM' [("A",ValIndex 1),("B",ValNull),("C",ValNull)]
--   --                [(1,vmkAVM [("A",ValAtom "y"),("B",ValIndex 1),("C",ValAtom "z")])]
--   pp "b" b
--   pp "c" c
--   putStrLn "b ⊔? c"
--   print $ b ⊔? c
--   let a = b ⊔ c
--   pp "a = b ⊔ c" $ a
--   putStrLn "b ⊑ a"
--   print $ b ⊑ a
--   putStrLn "c ⊑ a"
--   print $ c ⊑ a

cx_merging_dicts = do
  let
    a = mkAVM'
        [ ("A",ValIndex 1) ]
        [ (1, vmkAVM1 "X" (ValAtom "x")) ]
    b = mkAVM'
        [ ("B",ValIndex 1) ]
        [ (1, vmkAVM1 "Y" (ValAtom "y")) ]
  pp "a" a
  pp "b" b
  pp "a ⊔ b" (a ⊔ b)

-- cx_gen_absorption = do
--   -- a ⊑ b ==> a ⊓ b ~= a
--   let
--     a = mkAVM [("B",vnullAVM)]
--     b = mkAVM [("B",vmkAVM [("C",ValList [])])]

--   pp "a" a
--   pp "b" b
--   print (a ⊑ b)
--   pp "a ⊓ b" (a ⊓ b)

------------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary AVM where
  arbitrary = do
    body <- arbitrary
    let indices = getIndices (AVM body M.empty)
    vs :: [Value] <- mapM (\_ -> arbitrary `suchThat` (not.isIndex)) indices
    let dict = M.fromList (zip indices vs)
    -- TODO: allow for non-bound variables
    return $ AVM body dict
  shrink avm = [ AVM b (dictTrim b) | b <- shrink (avmBody avm) ]
    where
      -- Remove stuff from dicts which have been removed from body
      dictTrim :: AVMap -> Dict
      dictTrim m = M.intersection (avmDict avm) (M.fromList [(i,ValAtom "dummy") | i <- getIndices (AVM m M.empty)])

instance (Ord a, Arbitrary a, Arbitrary v) => Arbitrary (M.Map a v) where
  arbitrary = arbitrary >>= return . M.fromList
  shrink m = [ M.fromList l | l <- shrink (M.toList m) ] -- too many combos here?

instance Arbitrary Attribute where
  arbitrary = arbitraryAttr >>= return . Attr
  shrink _ = []

instance Arbitrary Value where
  arbitrary = sized arb
    where
      arb :: Int -> Gen Value
      arb n = oneof
              [ resize (n `div` 2) arbitraryAVMNoDict >>= return . ValAVM
              , arbitraryAtom >>= return . ValAtom
              -- , resize (n `div` 2) arbitrary >>= return . ValList
              , arbitraryIndex >>= return . ValIndex
              -- , return ValNull
              ]
  shrink v = case v of
    ValAVM avm -> map ValAVM (shrink avm)
    -- ValList vs -> map ValList (shrink vs)
    _ -> []

-- | Arbitrary AVM with empty dictionary
arbitraryAVMNoDict :: Gen AVM
arbitraryAVMNoDict = do
  avm <- arbitrary
  return $ AVM (avmBody avm) M.empty

-- | Arbitrary attribute name
arbitraryAttr :: Gen String
arbitraryAttr = elements ["A","B","C"]

-- | Arbitrary atomic value
arbitraryAtom :: Gen String
arbitraryAtom = elements ["x","y","z"]

-- | Arbitrary index
arbitraryIndex :: Gen Int
arbitraryIndex = choose (1,5)

------------------------------------------------------------------------------
-- Unit test cases

assert :: Bool -> IO ()
assert True  = putStrLn "Ok"
-- assert False = error "Assertion failed"
assert False = putStrLn "Assertion failed"

-- Subsumption
-- http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf

suite_sub :: IO ()
suite_sub = do
  let
    null = mkAVM []
    num_sg = mkAVM1 "NUM" (ValAtom "sg")
    num_pl = mkAVM1 "NUM" (ValAtom "pl")
    num_X  = mkAVM1 "NUM" (ValIndex 1)
    num_sg_per_3 = mkAVM [("NUM",ValAtom "sg"),("PERS",ValAtom "third")]
    per_3 = mkAVM1 "PERS" (ValAtom "third")
    num12 = mkAVM [("NUM1",ValAtom "sg"),("NUM2",ValAtom "sg")]
    num12' = mkAVM' [("NUM1",ValIndex 1),("NUM2",ValIndex 1)] [(1,ValAtom "sg")]

  assert $ null ⊑ num_sg
  assert $ num_X ⊑ num_sg
  assert $ num_sg ⊑ num_sg_per_3
  assert $ num12 ⊑ num12'

  assert $ not (subsumable num_sg num_pl)
  assert $ not (subsumable num_sg per_3)

-- Unification

suite_uni :: IO ()
suite_uni = do

  -- http://www3.cs.stonybrook.edu/~ychoi/cse507/slides/08-unification.pdf
  let
    sg = mkAVM [("NUMBER",ValAtom "sg")]
    pl = mkAVM [("NUMBER",ValAtom "pl")]
    nonum = mkAVM [("NUMBER",vnullAVM)]
    p3 = mkAVM [("PERSON",ValAtom "p3")]

    num_sg_per_p3 = vmkAVM [("NUMBER",ValAtom "sg"),("PERSON",ValAtom "p3")]
    num_pl_per_p3 = vmkAVM [("NUMBER",ValAtom "pl"),("PERSON",ValAtom "p3")]

    eg1a = mkAVM'
           [ ("AGREEMENT",ValIndex 1)
           , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (ValIndex 1)) ]
           [ (1, num_sg_per_p3) ]
    eg1b = mkAVM1 "SUBJECT" $ vmkAVM1 "AGREEMENT" num_sg_per_p3

    eg2a = mkAVM
           [ ("AGREEMENT",ValIndex 1)
           , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (ValIndex 1)) ]
    eg2b = mkAVM1 "SUBJECT" $ vmkAVM1 "AGREEMENT" num_sg_per_p3

    eg3a = mkAVM'
           [ ("AGREEMENT",ValIndex 1)
           , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (ValIndex 1)) ]
           [ (1, num_sg_per_p3) ]
    eg3b = mkAVM
           [ ("AGREEMENT",num_sg_per_p3)
           , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" num_pl_per_p3) ]

    eg4a = mkAVM
           [ ("AGREEMENT",vmkAVM1 "NUMBER" (ValAtom "sg"))
           , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (vmkAVM1 "NUMBER" (ValAtom "sg"))) ]
    eg4b = mkAVM1 "SUBJECT" (vmkAVM1 "AGREEMENT" num_sg_per_p3)

  assert $ (sg ⊔ sg) == sg
  assert $ not (sg ⊔? pl)
  assert $ sg ⊔ nonum == sg
  assert $ (sg ⊔ p3) == AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}
  assert $ (eg1a ⊔ eg1b) == AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1),(Attr "SUBJECT",ValAVM (AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1)], avmDict = M.fromList []}))], avmDict = M.fromList [(1,ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}))]}
  assert $ (eg2a ⊔ eg2b) == AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1),(Attr "SUBJECT",ValAVM (AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1)], avmDict = M.fromList []}))], avmDict = M.fromList [(1,ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}))]}
  assert $ not (eg3a ⊔? eg3b)
  assert $ (eg4a ⊔ eg4b) == AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg")], avmDict = M.fromList []})),(Attr "SUBJECT",ValAVM (AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}))], avmDict = M.fromList []}))], avmDict = M.fromList []}

suite_uni_2 :: IO ()
suite_uni_2 = do
  -- http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
  let
    a = mkAVM [ ("F",vmkAVM1 "NUM" (ValAtom "sg"))
              , ("G",vmkAVM1 "PERS" (ValAtom "third")) ]
    b = mkAVM [ ("F",ValIndex 1)
              , ("G",ValIndex 1) ]
    aub = mkAVM' [ ("F",ValIndex 1)
                 , ("G",ValIndex 1) ]
                 [ (1, vmkAVM [("NUM", ValAtom "sg"), ("PERS", ValAtom "third")] )]
  assert $ a ⊔ b == aub

  let
    a = mkAVM [ ("F",ValIndex 1) ]
    b = mkAVM [ ("F",vmkAVM1 "H" (ValAtom "b")) ]
    aub = mkAVM' [ ("F",ValIndex 1) ]
                 [ (1, vmkAVM1 "H" (ValAtom "b")) ]
  assert $ a ⊔ b == aub

  let
    num_sg  = vmkAVM [("NUM",ValAtom "sg")]
    per_3rd = vmkAVM [("PERS",ValAtom "3rd")]
    num_pers = vmkAVM [("NUM",ValAtom "sg"),("PERS",ValAtom "3rd")]
    a = mkAVM [ ("F",num_sg), ("G",num_sg) ]
    b = mkAVM [ ("F",per_3rd) ]
    aub = mkAVM [ ("F",num_pers)
                , ("G",num_sg) ]
    a' = mkAVM' [ ("F",ValIndex 1)
                , ("G",ValIndex 1) ]
                [ (1,num_sg) ]
    a'ub = mkAVM' [ ("F",ValIndex 1)
                  , ("G",ValIndex 1) ]
                  [ (1,num_pers) ]
  assert $ a ⊔ b == aub
  assert $ a' ⊔ b == a'ub

suite_uni_bind :: IO ()
suite_uni_bind = do
  -- http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
  let
    num_sg  = vmkAVM [("NUM",ValAtom "sg")]
    per_3rd = vmkAVM [("PERS",ValAtom "3rd")]
    num_per = vmkAVM [("NUM",ValAtom "sg"),("PERS",ValAtom "3rd")]
    a = mkAVM' [ ("F",ValIndex 1) ]
               [ (1, num_sg) ]
    b = mkAVM' [ ("F",ValIndex 2) ]
               [ (2, per_3rd) ]
    aub = mkAVM' [ ("F",ValIndex 1) ]
                 [ (1, num_per) ]
  -- putStrLn $ ppAVM $ a ⊔ b
  -- putStrLn $ ppAVM $ aub
  assert $ a ⊔ b ~= aub

suite_gen :: IO ()
suite_gen = do
  -- http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf
  let
    num_sg  = mkAVM [("NUM",ValAtom "sg")]
    num_pl  = mkAVM [("NUM",ValAtom "pl")]
    per_3rd = mkAVM [("PERS",ValAtom "3rd")]
    num_per = mkAVM [("NUM",ValAtom "sg"),("PERS",ValAtom "3rd")]
    agr_num_sg = mkAVM [("AGR",ValAVM num_sg)]

    x = mkAVM' [("F",ValIndex 1),("G",ValIndex 1)] [(1,ValAVM num_sg)]
    y = mkAVM [("F",ValAVM num_sg),("G",ValAVM num_sg)]

  assert $ num_sg ⊓ per_3rd ~= nullAVM
  assert $ num_sg ⊓ num_pl ~= nullAVM
  assert $ num_sg ⊓ num_per ~= num_sg
  assert $ nullAVM ⊓ agr_num_sg ~= nullAVM
  assert $ x ⊓ y ~= y

suite_val :: IO()
suite_val = do
  let
    mavm = mkMultiAVM
             [ vmkAVM [("F",vmkAVM [("G", ValAtom "a"),("H", ValIndex 1)])]
             , vmkAVM [("G",ValIndex 2)]
             , vmkAVM [("F",vmkAVM [("H", ValAtom "b"),("G", ValIndex 1)]), ("H",ValAtom "a")]
             ]
    v :: Int -> [String] -> Value
    v i as = fromJust $ mval (map Attr as) mavm i

  assert $ (v 1 ["F"]) == (vmkAVM [("G", ValAtom "a"),("H", vnullAVM)])
  assert $ (v 3 ["F"]) == (vmkAVM [("H", ValAtom "b"),("G", vnullAVM)])
  assert $ (v 1 ["F","H"]) == (v 3 ["F","G"])

------------------------------------------------------------------------------
-- One to rule them all

regression :: IO ()
regression = do
  suite_sub
  suite_uni
  suite_uni_2
  suite_uni_bind
  suite_gen
  suite_val
