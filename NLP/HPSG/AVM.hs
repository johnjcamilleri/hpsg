{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NLP.HPSG.AVM where

import qualified Data.Map as M
import Data.List (nub)
import Data.Maybe
import Text.Printf (printf)

import qualified Control.Monad.State as CMS
import qualified Control.Monad.Writer as CMW
import qualified Control.Monad.Except as CME

import qualified Data.Traversable as Tr

------------------------------------------------------------------------------
-- Types

-- | Attribute or feature, often drawn in CAPS
data Attribute = Attr String
  deriving (Eq, Ord, Show)

-- | Value of an attribute
data Value = ValAVM AVM      -- ^ A sub-structure (with its own dictionary - should always be factored up)
           | ValAtom Atom    -- ^ Atomic value
           | ValList [Value] -- ^ List of values
           | ValIndex Index  -- ^ Index to structure in dict
           | ValNull         -- ^ Often given as empty list []
  deriving (Eq, Ord, Show)

-- | Used for structure sharing
type Index = Int

-- | Atom. Perhaps this should be a parameter of the AVM type
type Atom = String

-- | An attribute-value matrix, with bound variables
data AVM = AVM {
  -- ^ The inner AVM
  avmBody :: AVMap,
  -- ^ Dictionary used for structure sharing
  avmDict :: Dict
  }
  deriving (Eq, Ord, Show)

-- | A map of attribute-values
type AVMap = M.Map Attribute Value

-- | The dictionary type
type Dict = M.Map Index Value

------------------------------------------------------------------------------
-- Helpers

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

-- | Are two AVM's dictionaries distinct?
--   Helpful in test case generation
distinctDicts :: AVM -> AVM -> Bool
distinctDicts a b =
  M.intersection (avmDict a) (avmDict b) == M.empty

------------------------------------------------------------------------------
-- Builders

type AVList = [(String,Value)]

attrList :: [(String,v)] -> [(Attribute,v)]
attrList = map (\(a,v)->(Attr a,v))

attrMap :: [(String,v)] -> M.Map Attribute v
attrMap = M.fromList . attrList

nullAVM = mkAVM []

-- -- | Make an AV map
-- mkAV :: AVList -> AVMap
-- mkAV = attrMap

-- -- | Make an AV map as a value
-- vmkAV :: AVList -> Value
-- vmkAV = ValAVM . mkAVM

-- | Make an AVM with a single attribute
mkAVM1 :: String -> Value -> AVM
mkAVM1 a v = mkAVM [(a,v)]

-- | Make an AVM with a single attribute as a Value
vmkAVM1 :: String -> Value -> Value
vmkAVM1 a v = ValAVM $ mkAVM1 a v

-- | Make an AVM with no name and empty dictionary
mkAVM :: AVList -> AVM
mkAVM l = AVM (attrMap l) M.empty

-- | Make an AVM with no name and a dictionary
mkAVM' :: AVList -> [(Index,Value)] -> AVM
mkAVM' l d = AVM (attrMap l) (M.fromList d)

-- | Make an AVM as a Value
vmkAVM :: AVList -> Value
vmkAVM = ValAVM . mkAVM

-- -- | Make an AVM with a name
-- mkAVMNamed :: Sort -> [(Attribute,Value)] -> AVM
-- mkAVMNamed name l = AVM (M.fromList ((Attr "SORT",ValAtom name):l)) M.empty

------------------------------------------------------------------------------
-- Pretty print

showValue :: (CMW.MonadWriter String m) => Value -> (AVM -> m ()) -> m ()
showValue v f =
  case v of
    ValAVM avm ->
      if M.size (avmDict avm) > 0
      then error "Non-empty middle dictionary"
      else f avm
    ValAtom s  -> CMW.tell s
    ValList vs -> CMW.tell $ "<"++replicate (length vs) '.'++">"
    ValIndex i -> CMW.tell $ "#"++show i
    ValNull    -> CMW.tell "[]"

ppAVM :: AVM -> String
ppAVM avm = CMW.execWriter f
  where
    putStr s   = CMW.tell s
    putStrLn s = CMW.tell (s++"\n")

    f :: (CMW.MonadWriter String m) => m ()
    f = do
      go 0 (avmBody avm)
      CMW.tell "\n"
      if M.size (avmDict avm) > 0
      then do
        putStrLn "where"
        mapM_ (\(i,val) -> do
                               putStr $ show i ++ " = "
                               showValue val (CMW.tell . ppAVM)
                               putStr "\n"
              ) (M.toList $ avmDict avm)
      else return ()

    go :: (CMW.MonadWriter String m) => Int -> AVMap -> m ()
    go l av = do
      putStr $ "["
      -- case M.lookup (Attr "SORT") (avmBody avm) of
      --   Just (ValAtom s) -> putStrLn $ (replicate l ' ') ++ ("[" ++ s)
      --   _ -> return ()
      mapM_ (uncurry $ f (l+1)) $ zip [0..] (M.toList av)
      where
        f :: (CMW.MonadWriter String m) => Int -> Int -> (Attribute,Value) -> m ()
        f l' i (Attr a,v) = do
          if i > 0
          then putStr $ replicate l' ' '
          else return ()
          putStr $ a++":"
          showValue v (\avm -> go (l'+(length a)+1) (avmBody avm))
          if i+1 == M.size av
          then putStr "]"
          else putStrLn " "

------------------------------------------------------------------------------
-- Identity

-- | Type Identity: checks values are same (including variables)
--   Often written =
typeIdentity :: Value -> Value -> Bool
typeIdentity a b = a == b

-- | Token Identity: checks things point to the same object
--   Basically only true when variables have same value
--   Often written ≐
tokenIdentity :: Value -> Value -> Bool
tokenIdentity (ValIndex i1) (ValIndex i2) | i1 == i2 = True
tokenIdentity _ _ = False

-- | Equality that follows reentrants
(~=) = eq
infix 4 ~= -- same as ==

eq a b | (M.keys (avmBody a)) /= (M.keys (avmBody b)) = False
eq a b =
  all (\k -> M.member k b2 && eqV (b1 M.! k) (b2 M.! k)) (M.keys b1)
  where
    (b1,d1) = (avmBody a, avmDict a)
    (b2,d2) = (avmBody b, avmDict b)

    eqV :: Value -> Value -> Bool
    eqV (ValIndex i1) v2 | M.member i1 d1 = eqV (d1 M.! i1) v2
    eqV v1 (ValIndex i2) | M.member i2 d2 = eqV v1 (d2 M.! i2)
    eqV v1 v2 = v1 == v2

------------------------------------------------------------------------------
-- Unification

-- | Unification
--   Throws an error if unification fails
(&) :: AVM -> AVM -> AVM
(&) a b = case unify a b of
  Left err -> error err
  Right avm -> avm

infixl 6 & -- same as +

-- | Unifiable
(&?) :: AVM -> AVM -> Bool
(&?) a b = case unify a b of
  Left _ -> False
  Right _ -> True

infix 4 &? -- same as <

-- | Unification, with helpful error messages
unify :: AVM -> AVM -> Either String AVM
unify a1 a2 = do
  (body, (d1, d2)) <- CMS.runStateT (s a1 a2) (avmDict a1, avmDict a2)
  return $ AVM body (dictMerge d1 d2)

  where
    s :: (CME.MonadError String m, CMS.MonadState (Dict,Dict) m) => AVM -> AVM -> m AVMap
    s a1 a2 = unionWithM f (avmBody a1) (avmBody a2)

    f :: (CME.MonadError String m, CMS.MonadState (Dict,Dict) m) => Value -> Value -> m Value

    -- Consider indices first
    -- TODO: this is still buggy
    f (ValIndex i1) v2 = do
      (d1,d2) <- CMS.get
      mv1 <- CMS.gets (\(d1,d2) -> M.lookup i1 d1)
      case mv1 of
        Just v1 -> do
          v1' <- f v1 v2
          CMS.modify (\(d1,d2) -> (M.insert i1 v1' d1, d2))
        Nothing ->
          CMS.modify (\(d1,d2) -> (M.insert i1 v2 d1, d2))
      return $ ValIndex i1
    f v1 (ValIndex i2) = do
      mv2 <- CMS.gets (\(d1,d2) -> (M.lookup i2 d2))
      case mv2 of
        Just v2 -> do
          v2' <- f v1 v2
          CMS.modify (\(d1,d2) -> (d1, M.insert i2 v2' d2))
        Nothing ->
          CMS.modify (\(d1,d2) -> (d1, M.insert i2 v1 d2))
      return $ ValIndex i2

    -- General equality
    f v1 v2 | v1 == v2 = return v1
    f (ValAVM av1) (ValAVM av2)
      | (avmDict av1) /= M.empty = CME.throwError $ "Non-empty dictionary in value"
      | (avmDict av2) /= M.empty = CME.throwError $ "Non-empty dictionary in value"
      | otherwise = do avmap <- unionWithM f (avmBody av1) (avmBody av2)
                       return $ ValAVM $ AVM avmap M.empty
    f (ValAVM av1) v2 | (avmBody av1) == M.empty = return v2
    f v1 (ValAVM av2) | (avmBody av2) == M.empty = return v1
    f (ValAtom a1) (ValAtom a2) | a1 == a2  = return $ ValAtom a1
                                | otherwise = CME.throwError $ printf "Cannot unify: %s and %s" a1 a2
    f (ValList l1) (ValList l2) = return $ ValList (l1++l2)
    f ValNull v2 = return v2
    f v1 ValNull = return v1
    f v1 v2 = CME.throwError $ printf "Cannot unify:\n  %s\n  %s" (show v1) (show v2)

    dictMerge :: Dict -> Dict -> Dict
    dictMerge = M.unionWithKey (\k _ -> error $ "Conflicting key: "++show k)

-- See: http://stackoverflow.com/a/19896320/98600
unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f mapA mapB =
  Tr.sequence $ M.unionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

------------------------------------------------------------------------------
-- Subsumption

-- | Are two AVMs comparable for subsumption ("subsumable")?
subsumable :: AVM -> AVM -> Bool
subsumable a b = a |< b || a |> b
-- subsumable a b = a |< b || b |< a

(|<) :: AVM -> AVM -> Bool
(|<) = subsumes
infix 4 |< -- same as <

(|>) :: AVM -> AVM -> Bool
(|>) = flip subsumes
infix 4 |> -- same as >

-- | AVMs may subsume eachother
--   A subsumes B (A is more general than B)
--   B is subsumed by A (B is more specific than A)
subsumes :: AVM -> AVM -> Bool
subsumes a b =
  all (\k -> M.member k b2 && subV (b1 M.! k) (b2 M.! k)) (M.keys b1)
  where
    (b1,d1) = (avmBody a, avmDict a)
    (b2,d2) = (avmBody b, avmDict b)

    subV :: Value -> Value -> Bool
    subV (ValIndex i1) v2 | M.member i1 d1 = subV (d1 M.! i1) v2
    subV v1 (ValIndex i2) | M.member i2 d2 = subV v1 (d2 M.! i2)
    subV v1 v2 = v1 == v2
