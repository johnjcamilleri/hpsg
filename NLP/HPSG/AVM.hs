{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NLP.HPSG.AVM where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Text.Printf (printf)
import Debug.Trace (trace)

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

-- | Lookup in AVM dictionary, as deep as necessary
--   It is valid to have non-bound variables
lookupAVM :: Index -> AVM -> Maybe Value
lookupAVM i avm = lookupDict i (avmDict avm)

-- | Lookup in AVM dictionary, as deep as necessary
--   It is valid to have non-bound variables
lookupDict :: Index -> Dict -> Maybe Value
lookupDict i dict =
  case M.lookup i dict of
    Just (ValIndex j) -> lookupDict j dict
    x -> x

-- | Is a value an index? Useful as a filter function
isIndex :: Value -> Bool
isIndex v = case v of
  ValIndex _ -> True
  _ -> False

-- | Get indices in the AVM dictionary
getDictIndices :: AVM -> [Index]
getDictIndices avm = M.keys (avmDict avm)

-- | Get indices in the AVM body
getIndices :: AVM -> [Index]
getIndices avm = L.nub $ CMW.execWriter (f avm)
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

-- | Do two AVM's use the same indices?
--   Helpful in test case generation
distinctIndices :: AVM -> AVM -> Bool
distinctIndices a b =
  L.intersect (getIndices a) (getIndices b) == []

-- | Go over AVM and clear middle dictionaries.
--   This involves pushing them up to the top level, possibly with renaming.
cleanMiddleDicts :: AVM -> AVM
cleanMiddleDicts avm = AVM b' d'
  where
    (b',d') = CMS.runState (s (avmBody avm)) (avmDict avm)
    s :: (CMS.MonadState Dict m) => AVMap -> m AVMap
    s = mapWithKeyM (const f)

    f :: (CMS.MonadState Dict m) => Value -> m Value
    f v@(ValAVM (AVM b d))
      | M.null d  = return v
      | otherwise = do
          dict <- CMS.get
          let is = [1..] L.\\ M.keys dict         -- available indices
              rs = M.fromList $ zip (M.keys d) is -- map of replacements
          CMS.modify (M.union (M.fromList [ (rs M.! k,v) | (k,v) <- M.toList d]))
          let b' = replaceIndices rs b
              newAVM = cleanMiddleDicts $ AVM b' M.empty
          return $ ValAVM newAVM
    f (ValList vs) = do
      vs' <- mapM f vs
      return $ ValList vs'
    f v = return v

replaceIndices :: M.Map Index Index -> AVMap -> AVMap
replaceIndices rs = M.map f
  where
    f :: Value -> Value
    f v = case v of
      -- ValAVM avm -> ValAVM
      ValIndex i -> ValIndex $ rs M.! i
      ValList vs -> ValList $ map f vs
      _ -> v

------------------------------------------------------------------------------
-- Builders

type AVList = [(String,Value)]

attrList :: [(String,v)] -> [(Attribute,v)]
attrList = map (\(a,v)->(Attr a,v))

attrMap :: [(String,v)] -> M.Map Attribute v
attrMap = M.fromList . attrList

-- | The empty AVM
nullAVM :: AVM
nullAVM = mkAVM []

-- | The empty AVM as a value
vnullAVM :: Value
vnullAVM = ValAVM nullAVM

-- | Make an AVM with a single attribute
mkAVM1 :: String -> Value -> AVM
mkAVM1 a v = mkAVM [(a,v)]

-- | Make an AVM with a single attribute as a Value
vmkAVM1 :: String -> Value -> Value
vmkAVM1 a v = ValAVM $ mkAVM1 a v

-- | Make an AVM with an empty dictionary
mkAVM :: AVList -> AVM
mkAVM l = AVM (attrMap l) M.empty

-- | Make an AVM with a dictionary
mkAVM' :: AVList -> [(Index,Value)] -> AVM
mkAVM' l d = AVM (attrMap l) (M.fromList d)

-- | Make an AVM as a Value
vmkAVM :: AVList -> Value
vmkAVM = ValAVM . mkAVM

-- | Make an AVM with a dictionary as a Value
vmkAVM' :: AVList -> [(Index,Value)] -> Value
vmkAVM' l d = ValAVM $ mkAVM' l d

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
    ValList vs -> do
      CMW.tell "<"
      if null vs
      then return ()
      else do
        let f avm = CMW.tell "."
        showValue (head vs) f
        mapM_ (\v -> CMW.tell "," >> showValue v f) (tail vs)
      CMW.tell ">"
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
  -- if not (distinctIndices a2 a2)
  -- then CME.throwError "I don't want to unify AVMs with intersecting indices"
  -- else return nullAVM

  dict <- dictMerge (avmDict a1) (avmDict a2)
  (body, dict2) <- CMS.runStateT (s a1 a2) dict
  return $ AVM body dict2

  where
    s :: (CME.MonadError String m, CMS.MonadState Dict m) => AVM -> AVM -> m AVMap
    s a1 a2 = unionWithM f (avmBody a1) (avmBody a2)

    f :: (CME.MonadError String m, CMS.MonadState Dict m) => Value -> Value -> m Value

    -- Consider indices first
    -- TODO: this is still buggy
    f (ValIndex i1) (ValIndex i2) = do
      mv1 <- CMS.gets (lookupDict i1)
      mv2 <- CMS.gets (lookupDict i2)
      case (mv1,mv2) of
        (Just v1,Just v2) -> do
          v' <- f v1 v2
          CMS.modify (M.insert i1 v')
          return $ ValIndex i1
        (Just v1,Nothing) -> return $ ValIndex i1
        (Nothing,Just v2) -> return $ ValIndex i2
        (Nothing,Nothing) -> return $ ValIndex i1 -- is that right?

    f (ValIndex i1) v2 = do
      mv1 <- CMS.gets (lookupDict i1)
      case mv1 of
        Just v1 -> do
          v1' <- f v1 v2
          CMS.modify (M.insert i1 v1')
        Nothing ->
          CMS.modify (M.insert i1 v2)
      return $ ValIndex i1
    f v1 (ValIndex i2) = do
      mv2 <- CMS.gets (lookupDict i2)
      case mv2 of
        Just v2 -> do
          v2' <- f v1 v2
          CMS.modify (M.insert i2 v2')
        Nothing ->
          CMS.modify (M.insert i2 v1)
      return $ ValIndex i2
    -- f (ValIndex i1) v2 = do
    --   (d1,d2) <- CMS.get
    --   mv1 <- CMS.gets (\(d1,d2) -> lookupDict i1 d1)
    --   case mv1 of
    --     Just v1 -> do
    --       v1' <- f v1 v2
    --       CMS.modify (\(d1,d2) -> (M.insert i1 v1' d1, d2))
    --     Nothing ->
    --       CMS.modify (\(d1,d2) -> (M.insert i1 v2 d1, d2))
    --   -- CMS.get >>= \s -> trace (show s) (return ValNull)
    --   return $ ValIndex i1
    -- f v1 (ValIndex i2) = do
    --   mv2 <- CMS.gets (\(d1,d2) -> lookupDict i2 d2)
    --   case mv2 of
    --     Just v2 -> do
    --       v2' <- f v1 v2
    --       CMS.modify (\(d1,d2) -> (d1, M.insert i2 v2' d2))
    --     Nothing ->
    --       CMS.modify (\(d1,d2) -> (d1, M.insert i2 v1 d2))
    --   -- CMS.get >>= \s -> trace (show s) (return ValNull)
    --   return $ ValIndex i2

    -- General equality
    f v1 v2 | v1 == v2 = return v1
    f (ValAVM av1) (ValAVM av2)
      | (avmDict av1) /= M.empty = CME.throwError $ "Non-empty dictionary in value"
      | (avmDict av2) /= M.empty = CME.throwError $ "Non-empty dictionary in value"
      | otherwise = do avmap <- unionWithM f (avmBody av1) (avmBody av2)
                       return $ ValAVM $ AVM avmap M.empty
    f (ValAVM av1) v2 | (avmBody av1) == M.empty = return v2
    f v1 (ValAVM av2) | (avmBody av2) == M.empty = return v1

    -- Empty lists should be treated like nulls
    -- f (ValList []) v2 = return v2
    -- f v1 (ValList []) = return v1
    -- Singleton lists should be treated like their head
    -- f (ValList [v1']) (ValList [v2']) = f v1' v2'
    -- f (ValList [v1']) v2 = f v1' v2
    -- f v1 (ValList [v2']) = f v1 v2'
    -- f (ValList l1) (ValList l2) = return $ ValList (l1++l2)

    f ValNull v2 = return v2
    f v1 ValNull = return v1
    f v1 v2 = CME.throwError $ printf "Cannot unify:\n  %s\n  %s" (show v1) (show v2)

    dictMerge :: (CME.MonadError String m) => Dict -> Dict -> m Dict
    dictMerge = unionWithKeyM f
      where
        f :: (CME.MonadError String m) => Index -> Value -> Value -> m Value
        f k v1 v2 = if v1==v2
                    then return v1
                    else CME.throwError $ printf "Conflict for key %s: %s and %s " (show k) (show v1) (show v2)

-- See: http://stackoverflow.com/a/19896320/98600
unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f mapA mapB =
  Tr.sequence $ M.unionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

unionWithKeyM :: (Monad m, Ord k) => (k -> a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithKeyM f mapA mapB =
  Tr.sequence $ M.unionWithKey (\k a b -> do {x <- a; y <- b; f k x y}) (M.map return mapA) (M.map return mapB)

mapWithKeyM :: (Monad m, Ord k) => (k -> a -> m a) -> M.Map k a -> m (M.Map k a)
mapWithKeyM f mapA =
  Tr.sequence $ M.mapWithKey (\k a -> do {x <- a; f k x}) (M.map return mapA)

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
                          | otherwise      = True
    subV v1 (ValIndex i2) | M.member i2 d2 = subV v1 (d2 M.! i2)
                          | otherwise      = False -- ?
    subV ValNull v2 = True
    subV (ValAVM avm) v2  | avm == nullAVM = True
    -- subV v1 (ValAVM avm)  | avm == nullAVM = True
    subV v1 v2 = v1 == v2
