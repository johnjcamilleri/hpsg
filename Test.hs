{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import qualified Data.Map as M

import Test.QuickCheck
import qualified Control.Monad.Writer as CMW

import NLP.HPSG.AVM

------------------------------------------------------------------------------
-- Properties
-- Taken from http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf

prop_subsumption_least :: AVM -> Bool
prop_subsumption_least a = nullAVM |< a

prop_subsumption_reflexive :: AVM -> Bool
prop_subsumption_reflexive a = a |< a

prop_subsumption_transitive :: AVM -> AVM -> AVM -> Property
prop_subsumption_transitive a b c =
  a |< b && b |< c ==> a |< c

prop_subsumption_antisymmetric :: AVM -> AVM -> Property
prop_subsumption_antisymmetric a b =
  a |< b && b |< a ==> a ~= b

prop_unification_idempotent :: AVM -> Bool
prop_unification_idempotent a =
  a & a ~= a

prop_unification_commutative :: AVM -> AVM -> Property
prop_unification_commutative a b =
  a &? b && distinctDicts a b ==> a & b ~= b & a

prop_unification_associative :: AVM -> AVM -> AVM -> Property
prop_unification_associative a b c =
  (a &? b) && (b &? c) ==> a & (b & c) ~= (a & b) & c

prop_unification_absorbing :: AVM -> AVM -> Property
prop_unification_absorbing a b =
  a |< b ==> a & b ~= b

prop_unification_monotonic :: AVM -> AVM -> AVM -> Property
prop_unification_monotonic a b c =
  a |< b ==> (a & c) |< (b & c)

props = do
  let args = stdArgs
        { maxSize = 10 -- default = 100
        , chatty = True -- default = True
        , maxSuccess = 20 -- default = 100
        , maxDiscardRatio = 100 -- default = 10
        }

  putStrLn "[ Subsumption ]"
  putStrLn "Least element" >> quickCheckWith args prop_subsumption_least
  putStrLn "Reflexivity" >> quickCheckWith args prop_subsumption_reflexive
  putStrLn "Transitivity" >> quickCheckWith args prop_subsumption_transitive
  putStrLn "Anti-symmetry" >> quickCheckWith args prop_subsumption_antisymmetric

  putStrLn "[ Unification ]"
  -- putStrLn "Idempotency" >> quickCheckWith args prop_unification_idempotent
  putStrLn "Commutativity" >> quickCheckWith args prop_unification_commutative
  -- putStrLn "Associativity" >> quickCheckWith args prop_unification_associative
  putStrLn "Absorption" >> quickCheckWith args prop_unification_absorbing
  -- putStrLn "Monotinicity" >> quickCheckWith args prop_unification_monotonic

------------------------------------------------------------------------------
-- Counter-examples

cx_commutativity = do
  let a = AVM {avmBody = M.fromList [(Attr "B",ValIndex 3),(Attr "C",ValNull)], avmDict = M.fromList [(3,ValAtom "y")]}
  let b = AVM {avmBody = M.fromList [(Attr "B",ValIndex 5),(Attr "C",ValIndex 5)], avmDict = M.fromList [(5,ValNull)]}
  putStrLn $ ppAVM a
  putStrLn $ ppAVM b
  putStrLn $ ppAVM (a & b)

cx_merging_dicts = do
  let
    a = mkAVM'
        [ ("A",ValIndex 1) ]
        [ (1, vmkAVM1 "X" (ValAtom "x")) ]
    b = mkAVM'
        [ ("B",ValIndex 1) ]
        [ (1, vmkAVM1 "Y" (ValAtom "y")) ]
  putStrLn $ ppAVM a
  putStrLn $ ppAVM b
  putStrLn $ ppAVM (a & b)

------------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary AVM where
  arbitrary = do
    body <- arbitrary
    let indices = getIndices (AVM body M.empty)
    vs :: [Value] <- mapM (\_ -> arbitrary `suchThat` (not.isIndex)) indices
    let dict = M.fromList (zip indices vs)
    return $ AVM body dict

instance (Ord a, Arbitrary a, Arbitrary v) => Arbitrary (M.Map a v) where
  arbitrary = do
    l <- arbitrary
    return $ M.fromList l

instance Arbitrary Attribute where
  arbitrary = do
    s <- arbitraryAttr
    return $ Attr s

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
