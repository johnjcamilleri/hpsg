{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import qualified Data.Map as M

import Test.QuickCheck
import qualified Control.Monad.Writer as CMW

import NLP.HPSG.AVM

------------------------------------------------------------------------------
-- Properties
-- Taken from http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf

prop_equality_commutative :: AVM -> AVM -> Property
prop_equality_commutative a b =
  a ~= b ==> b ~= a

prop_subsumption_least :: AVM -> Bool
prop_subsumption_least a = nullAVM |< a

prop_subsumption_reflexive :: AVM -> Bool
prop_subsumption_reflexive a = a |< a

prop_subsumption_transitive :: AVM -> AVM -> AVM -> Property
prop_subsumption_transitive a b c =
  (a |< b) && (b |< c) ==> a |< c

prop_subsumption_antisymmetric :: AVM -> AVM -> Property
prop_subsumption_antisymmetric a b =
  (a |< b) && (b |< a) ==> a ~= b

prop_subsumes_implies_unifiable :: AVM -> AVM -> Property
prop_subsumes_implies_unifiable a b =
  a |< b ==> a &? b

prop_unification_idempotent :: AVM -> Bool
prop_unification_idempotent a =
  (a &? a) && (a & a ~= a)

prop_unification_commutative :: AVM -> AVM -> Property
prop_unification_commutative a b =
  (a &? b) && (distinctDicts a b) ==> (b &? a) && (a & b ~= b & a)

prop_unification_associative :: AVM -> AVM -> AVM -> Property
prop_unification_associative a b c =
  and
    [ (a &? b)
    , (b &? c)
    , (a &? (b & c))
    , ((a & b) &? c)
    , distinctDicts a b
    , distinctDicts a c
    , distinctDicts b c
    ] ==> a & (b & c) ~= (a & b) & c

prop_unification_absorbing :: AVM -> AVM -> Property
prop_unification_absorbing a b =
  a |< b ==> a & b ~= b

prop_unification_monotonic :: AVM -> AVM -> AVM -> Property
prop_unification_monotonic a b c =
  a |< b ==> (a & c) |< (b & c)

prop_unification_most_general :: AVM -> AVM -> Property
prop_unification_most_general b c =
  b &? c ==> let a = b & c in (b |< a) && (c |< a)

props = do
  let args = stdArgs
        { maxSize = 10          -- default = 100
        , chatty = True         -- default = True
        , maxSuccess = 100      -- default = 100
        , maxDiscardRatio = 100 -- default = 10
        }

  -- gives up!
  -- putStrLn "[ Equality ]"
  -- putStrLn "Commutativity" >> quickCheckWith args prop_equality_commutative

  -- all pass ok...
  -- putStrLn "[ Subsumption ]"
  -- putStrLn "Least element" >> quickCheckWith args prop_subsumption_least
  -- putStrLn "Reflexivity" >> quickCheckWith args prop_subsumption_reflexive
  -- putStrLn "Transitivity" >> quickCheckWith args prop_subsumption_transitive
  -- putStrLn "Anti-symmetry" >> quickCheckWith args prop_subsumption_antisymmetric
  -- putStrLn "Implies unifiable" >> quickCheckWith args prop_subsumes_implies_unifiable

  -- putStrLn "[ Unification ]"
  -- putStrLn "Idempotency" >> quickCheckWith args prop_unification_idempotent -- ok
  -- putStrLn "Commutativity" >> quickCheckWith args prop_unification_commutative
  -- putStrLn "Associativity" >> quickCheckWith args prop_unification_associative -- ok
  -- putStrLn "Absorption" >> quickCheckWith args prop_unification_absorbing
  -- putStrLn "Monotinicity" >> quickCheckWith args prop_unification_monotonic
  putStrLn "Most general" >> quickCheckWith args prop_unification_most_general

------------------------------------------------------------------------------
-- Counter-examples

pp s avm = do
  putStrLn $ "--- " ++ s ++ " ---"
  putStrLn $ ppAVM avm

-- cx_idempotency = do
--   let a = mkAVM' [("B",ValIndex 2)] [(2,ValAtom "z")]
--   pp "a" a
--   pp "a & a" $ a & a
--   assert $ a ~= a & a

cx_commutativity = do
  let a = mkAVM' [("C",ValIndex 5)] [(5,ValList [])]
  let b = mkAVM' [("C",ValIndex 3)] [(3,ValList [ValList [ValNull]])]
  pp "a" a
  pp "b" b
  pp "a & b" $ a & b
  pp "b & a" $ b & a
  assert $ a & b ~= b & a

cx_absorption = do
  let a = mkAVM' [("C",ValIndex 1)] [(1,ValNull)]
  let b = mkAVM' [("B",ValList [vmkAVM [("B",ValIndex 1)]]),("C",ValNull)] [(1,ValAtom "x")]
  pp "a" a
  pp "b" b
  pp "a & b" $ a & b
  assert $ a |< b
  assert $ a & b ~= b

-- TODO:
cx_most_general = do
  -- b &? c ==> let a = b & c in (b |< a) && (c |< a)
  let b = mkAVM [("C",vmkAVM [("B",ValNull)] )]
  let c = mkAVM [("C",vmkAVM [("A",ValList [])] )]

  pp "b" b
  pp "c" c
  putStrLn "b &? c"
  print $ b &? c
  let a = b & c
  pp "a = b & c" $ a
  putStrLn "b |< a"
  print $ b |< a
  putStrLn "c |< a"
  print $ c |< a

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
  pp "a & b" (a & b)

------------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary AVM where
  arbitrary = do
    body <- arbitrary
    let indices = getIndices (AVM body M.empty)
    vs :: [Value] <- mapM (\_ -> arbitrary `suchThat` (not.isIndex)) indices
    let dict = M.fromList (zip indices vs)
    return $ cleanMiddleDicts $ AVM body dict
  shrink avm = [ AVM b (dictTrim b) | b <- shrink (avmBody avm) ]
    where
      -- Remove stuff from dicts which have been removed from body
      dictTrim :: AVMap -> Dict
      dictTrim m = M.intersection (avmDict avm) (M.fromList [(i,ValNull) | i <- getIndices (AVM m M.empty)])

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
              [ resize (n `div` 2) arbitrary >>= return . ValAVM
              , arbitraryAtom >>= return . ValAtom
              , resize (n `div` 2) arbitrary >>= return . ValList
              , arbitraryIndex >>= return . ValIndex
              , return ValNull
              ]
  shrink v = case v of
    ValAVM avm -> map ValAVM (shrink avm)
    ValList vs -> map ValList (shrink vs)
    _ -> []

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
assert False = error "Assertion failed"

-- General unification examples
-- http://www3.cs.stonybrook.edu/~ychoi/cse507/slides/08-unification.pdf

sg = mkAVM [("NUMBER",ValAtom "sg")]
pl = mkAVM [("NUMBER",ValAtom "pl")]
nonum = mkAVM [("NUMBER",ValNull)]
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

suite :: IO ()
suite = do
  assert $ (sg & sg) == sg
  assert $ not (sg &? pl)
  assert $ sg & nonum == sg
  assert $ (sg & p3) == AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}
  assert $ (eg1a & eg1b) == AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1),(Attr "SUBJECT",ValAVM (AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1)], avmDict = M.fromList []}))], avmDict = M.fromList [(1,ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}))]}
  assert $ (eg2a & eg2b) == AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1),(Attr "SUBJECT",ValAVM (AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValIndex 1)], avmDict = M.fromList []}))], avmDict = M.fromList [(1,ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}))]}
  assert $ not (eg3a &? eg3b)
  assert $ (eg4a & eg4b) == AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg")], avmDict = M.fromList []})),(Attr "SUBJECT",ValAVM (AVM {avmBody = M.fromList [(Attr "AGREEMENT",ValAVM (AVM {avmBody = M.fromList [(Attr "NUMBER",ValAtom "sg"),(Attr "PERSON",ValAtom "p3")], avmDict = M.fromList []}))], avmDict = M.fromList []}))], avmDict = M.fromList []}

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

  assert $ null |< num_sg
  assert $ num_X |< num_sg
  assert $ num_sg |< num_sg_per_3
  assert $ num12 |< num12'

  assert $ not (subsumable num_sg num_pl)
  assert $ not (subsumable num_sg per_3)

------------------------------------------------------------------------------
-- One to rule them all

regression :: IO ()
regression = do
  suite
  suite_sub
