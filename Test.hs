{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import qualified Data.Map as M
import Data.List (nub)

import Test.QuickCheck
import qualified Control.Monad.Writer as CMW

import NLP.HPSG.AVM

------------------------------------------------------------------------------
-- Properties
-- Taken from http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug2.pdf

prop_unification_idempotent :: AVM -> Bool
prop_unification_idempotent a =
  a & a == a

prop_unification_commutative :: AVM -> AVM -> Property
prop_unification_commutative a b =
  a &? b ==> a & b == b & a

prop_unification_associative :: AVM -> AVM -> AVM -> Property
prop_unification_associative a b c =
  (a &? b) && (b &? c) ==> a & (b & c) == (a & b) & c

prop_unification_absorbing :: AVM -> AVM -> Property
prop_unification_absorbing a b =
  a !< b ==> a & b == b

prop_unification_monotonic :: AVM -> AVM -> AVM -> Property
prop_unification_monotonic a b c =
  a !< b ==> (a & c) !< (b & c)

main = do
  let args = stdArgs { chatty = True, maxSuccess = 20 }
  -- quickCheckWith args prop_unification_idempotent
  quickCheckWith args prop_unification_commutative
  -- quickCheckWith args prop_unification_associative
  -- quickCheckWith args prop_unification_absorbing
  -- quickCheckWith args prop_unification_monotonic

-- Temp
a = AVM {avmBody = M.fromList [(Attr "B",ValIndex 3),(Attr "C",ValNull)], avmDict = M.fromList [(3,ValAtom "y")]}
b = AVM {avmBody = M.fromList [(Attr "B",ValIndex 5),(Attr "C",ValIndex 5)], avmDict = M.fromList [(5,ValNull)]}

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
  arbitrary = oneof
              -- [ arbitrary >>= return . ValAVM
              [ arbitraryAtom >>= return . ValAtom
              -- , arbitrary >>= return . ValList
              , arbitraryIndex >>= return . ValIndex
              , return ValNull
              ]

-- | Is a value an index? Useful as a filter function
isIndex :: Value -> Bool
isIndex v = case v of
  ValIndex _ -> True
  _ -> False

-- | Get indices in the AVM body
getIndices :: AVM -> [Index]
getIndices avm = nub $ CMW.execWriter (f avm)
  where
    f :: AVM -> CMW.Writer [Index] ()
    f avm = mapM_ g (M.elems (avmBody avm))
    g :: Value -> CMW.Writer [Index] ()
    g v = case v of
      ValAVM avm -> f avm
      ValList vs -> mapM_ g vs
      ValIndex i -> CMW.tell [i]
      _ -> return ()

-- | Arbitrary attribute name
arbitraryAttr :: Gen String
arbitraryAttr = elements ["A","B","C"]

-- | Arbitrary atomic value
arbitraryAtom :: Gen String
arbitraryAtom = elements ["x","y","z"]

-- | Arbitrary index
arbitraryIndex :: Gen Int
arbitraryIndex = choose (1,5)
