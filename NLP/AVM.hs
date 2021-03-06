{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Attribute-Value Matrices
module NLP.AVM where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Data.Either (partitionEithers)
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
           | ValIndex Index  -- ^ Index to structure in dict
  deriving (Eq, Ord, Show)

isValAVM   v = case v of ValAVM _ -> True ; _ -> False
isValAtom  v = case v of ValAtom _ -> True ; _ -> False
isValIndex v = case v of ValIndex _ -> True ; _ -> False

type Path = [Attribute]

-- | Used for structure sharing
type Index = Int

-- | Atom. Perhaps this should be a parameter of the AVM type
type Atom = String

-- | An attribute-value matrix, with bound variables
data AVM = AVM {
  avmBody :: AVMap, -- ^ The inner AVM
  avmDict :: Dict   -- ^ Dictionary used for structure sharing
  }
  deriving (Eq, Ord)
instance Show AVM where
  show = inlineAVM

-- | A map of attribute-values
type AVMap = M.Map Attribute Value

-- | The dictionary type
type Dict = M.Map Index Value

-- | Multi-AVM
data MultiAVM = MultiAVM {
  mavmBody :: [Value], -- ^ The inner AVMs or indices. Atoms here are illegal.
  mavmDict :: Dict     -- ^ Dictionary used for structure sharing
  }
  deriving (Eq, Ord)
instance Show MultiAVM where
  show = inlineMAVM

-- | Convert Multi-AVM to single AVM
--   Dictionary is copied into AVM
--   Indexes start from 1
--   Throws error if index is out of range
toAVM :: MultiAVM -> Int -> AVM
toAVM mavm i = AVM (body) (mavmDict mavm)
  where
    body = case (mavmBody mavm) !! (i-1) of
      ValAVM avm -> avmBody avm
      ValIndex ix -> case M.lookup ix (mavmDict mavm) of
        Just (ValAVM avm) -> avmBody avm -- dictionary?
        _ -> error $ "Cannot convert to AVM"
      _ -> error $ "Illegal MultiAVM: " ++ show mavm

------------------------------------------------------------------------------
-- Helpers

-- | val function as defined in http://cs.haifa.ac.il/~shuly/teaching/06/nlp/ug1.pdf
--   Except that unbound variables return a ValIndex, not Nothing
--   Nothing essentially means the path can't be resolved
val :: Path -> AVM -> Maybe Value
val p avm = case p of
  [] -> Nothing
  [a] -> val' a avm
  (a:as) -> case val' a avm of
    Just (ValAVM avm) -> val as avm
    _ -> Nothing

  where
    -- val for a single attribute
    val' :: Attribute -> AVM -> Maybe Value
    val' a avm = case M.lookup a (avmBody avm) of
        Just (ValIndex i) -> lookupDict' i (avmDict avm)
        x -> x

-- | val extended for multi-AVMs
--   Indexes start from 1
mval :: Path -> MultiAVM -> Int -> Maybe Value
mval p mavm i = val p (toAVM mavm i)

-- | Convert Haskell list to AVM-list
unlist :: [Value] -> Value
unlist [] = ValAtom "elist"
unlist (l:ls) = vmkAVM [("FIRST",l),("REST",unlist ls)]

-- | Convert AVM-list to Haskell list
--   Throws error if not well-formed
tolist :: Value -> [Value]
tolist (ValAtom "elist") = []
tolist (ValAVM avm) = v "FIRST" : tolist (v "REST")
  where
    v s = (avmBody avm) M.! (Attr s)

-- | Is an AVM actually an encoded list?
islist :: Value -> Bool
islist (ValAtom "elist") = True
islist (ValAVM avm) = L.sort (M.keys (avmBody avm)) == [Attr "FIRST", Attr "REST"] && islist (v "REST")
  where
    v s = (avmBody avm) M.! (Attr s)
islist _ = False

-- | Remove an attribute from an AVM
--   Only works one level deep
--   Does not follow indices
remAttr :: Attribute -> AVM -> (AVM,(Attribute,Maybe Value))
remAttr a avm = (AVM body' dict, (a,rem))
  where
    body' = M.delete a (avmBody avm)
    dict = avmDict avm
    rem = M.lookup a (avmBody avm)

-- | Add an attribute to an AVM
--   Only works one level deep
--   Does not follow indices
addAttr :: (Attribute,Maybe Value) -> AVM -> AVM
addAttr (_,Nothing) avm = avm
addAttr (a,Just v) avm = AVM body' dict
  where
    body' = M.insert a v (avmBody avm)
    dict = avmDict avm

------------------------------------------------------------------------------

-- | Lookup in AVM dictionary, as deep as necessary
--   It is valid to have non-bound variables
lookupAVM :: Index -> AVM -> Maybe Value
lookupAVM i avm = lookupDict i (avmDict avm)

-- | Lookup in AVM dictionary, as deep as necessary
--   Non-bound variables will return nothing
lookupDict :: Index -> Dict -> Maybe Value
lookupDict i dict =
  case M.lookup i dict of
    Just (ValIndex j) -> lookupDict j dict
    -- Just (ValIndex j) -> case lookupDict j dict of
    --   Just v -> Just v
    --   Nothing -> Just (ValIndex j)
    x -> x

-- | Lookup in AVM dictionary, as deep as necessary
--   Non-bound variables will return most recent index
lookupDict' :: Index -> Dict -> Maybe Value
lookupDict' i dict =
  case M.lookup i dict of
    Just (ValIndex j) -> lookupDict' j dict
    Nothing -> Just (ValIndex i)
    x -> x

-- | Is a value an index? Useful as a filter function
isIndex :: Value -> Bool
isIndex v = case v of
  ValIndex _ -> True
  _ -> False

-- | Get indices in the AVM
getIndices :: AVM -> [Index]
getIndices avm = L.nub $ fb ++ fd
  where
    fb = concatMap g (M.elems (avmBody avm))
    fd = concatMap g (M.elems (avmDict avm))

    g :: Value -> [Index]
    g v = case v of
      ValAVM avm -> getIndices avm
      ValIndex i -> i : case lookupAVM i avm of
                          Just v' -> g v'
                          Nothing -> []
      _ -> []

-- | Get a new, unique index for this AVM
newIndex :: AVM -> Index
newIndex avm = if null is
               then 1
               else 1 + (last is)
  where
    is = L.sort $ getIndices avm

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

-- | Merge two dictionaries, throwing an error for conflicts
mergeDicts :: Dict -> Dict -> Dict
mergeDicts = M.unionWithKey f
  where
    f :: Index -> Value -> Value -> Value
    f k v1 v2 = case mergeValues v1 v2 of
      Just v -> v
      Nothing -> error $ printf "mergeDicts: Conflict for key %s: %s and %s " (show k) (show v1) (show v2)

-- | Inner function for merging values
--   This should be used by all dict merges
mergeValues :: Value -> Value -> Maybe Value
mergeValues v1 v2 | visnull v1 = Just v2
mergeValues v1 v2 | visnull v2 = Just v1
mergeValues v1 v2 = if v1==v2
                    then Just v1
                    else Nothing

mergeValuesWithKey :: a -> Value -> Value -> Maybe Value
mergeValuesWithKey _ v1 v2 = mergeValues v1 v2

-- | Merge dict of second AVM into the first one
mergeAVMDicts :: AVM -> AVM -> AVM
mergeAVMDicts a b = AVM (avmBody a) (mergeDicts (avmDict a) (avmDict b))

-- | Can two dictionaries be merged?
canMergeDicts :: Dict -> Dict -> Bool
canMergeDicts a b = all snd $ M.toList $ M.intersectionWith f a b
  where
    f :: Value -> Value -> Bool
    f v1 v2 = isJust $ mergeValues v1 v2

-- | Can the dictionaries of two AVMs be merged?
canMergeAVMDicts :: AVM -> AVM -> Bool
canMergeAVMDicts a b = canMergeDicts (avmDict a) (avmDict b)

-- | Clear dictionary of an AVM
--   Should be done after merging upwards
cleanDict :: AVM -> AVM
cleanDict avm = AVM (avmBody avm) M.empty

-- | Set the dictionary of an AVM
setDict :: Dict -> AVM -> AVM
setDict dict avm = AVM (avmBody avm) dict

-- -- | Go over AVM and clear middle dictionaries.
-- --   This involves pushing them up to the top level, possibly with renaming.
-- cleanMiddleDicts :: AVM -> AVM
-- cleanMiddleDicts avm = AVM b' d'
-- -- TODO: this should also apply to AVMs in the dictionary
--   where
--     (b',d') = CMS.runState (s (avmBody avm)) (avmDict avm)
--     s :: (CMS.MonadState Dict m) => AVMap -> m AVMap
--     s = mapWithKeyM (const f)

--     f :: (CMS.MonadState Dict m) => Value -> m Value
--     f v@(ValAVM (AVM b d))
--       | M.null d  = return v
--       | otherwise = do
--           dict <- CMS.get
--           let is = [1..] L.\\ M.keys dict         -- available indices
--               rs = M.fromList $ zip (M.keys d) is -- map of replacements
--           CMS.modify (M.union (M.fromList [ (rs M.! k,v) | (k,v) <- M.toList d]))
--           let b' = replaceIndices rs b
--               newAVM = cleanMiddleDicts $ AVM b' M.empty
--           return $ ValAVM newAVM
--     f v = return v

-- -- | Rename/replace indices
-- replaceIndices :: M.Map Index Index -> AVMap -> AVMap
-- replaceIndices rs = M.map f
--   where
--     f :: Value -> Value
--     f v = case v of
--       ValIndex i -> ValIndex $ rs M.! i
--       _ -> v

-- | Rename/replace indices
reIndex :: M.Map Index Index -> AVM -> AVM
reIndex rs avm = AVM body' dict''
  where
    dict' = M.mapKeys (\k1 -> case M.lookup k1 rs of Just k2 -> k2 ; Nothing -> k1) (avmDict avm)
    dict'' = M.map f dict'
    body' = M.map f (avmBody avm)
    f :: Value -> Value
    f v = case v of
      ValIndex i -> ValIndex $ M.findWithDefault i i rs
      ValAVM avm -> ValAVM $ reIndex rs avm
      _ -> v

-- | Rename/replace indices in MultiAVM
mreIndex :: M.Map Index Index -> MultiAVM -> MultiAVM
mreIndex rs mavm = MultiAVM body' dict''
  where
    dict' = M.mapKeys (\k1 -> case M.lookup k1 rs of Just k2 -> k2 ; Nothing -> k1) (mavmDict mavm)
    dict'' = M.map f dict'
    body' = map f (mavmBody mavm)
    f :: Value -> Value
    f v = case v of
      ValIndex i -> ValIndex $ M.findWithDefault i i rs
      ValAVM avm -> ValAVM $ reIndex rs avm
      _ -> v

-- -- | Given two AVMs, try to figure out which indices where renamed
-- --   This would be best generated during unification, but i'm hoping this is a quick fix
-- --   Note the "direction" of the arguments
-- replacedIndices :: AVM -> AVM -> M.Map Index Index
-- replacedIndices a1 a2 = M.fromList $ L.nub $ catMaybes $ map f (paths a1)
--   where
--     f :: Path -> Maybe (Index, Index)
--     f p = case (val p a1, val p a2) of
--       (Just (ValIndex i1), Just (ValIndex i2)) -> if i1 /= i2 then Just (i1,i2) else Nothing
--       _ -> Nothing
--     -- Our own version of val which doesn't follow indices
--     val :: Path -> AVM -> Maybe Value
--     val [] avm = Nothing
--     val [a] avm = val' a avm
--     val (a:as) avm = case val' a avm of
--       Just (ValAVM avm') -> val as avm'
--       _ -> Nothing
--     val' a avm = M.lookup a (avmBody avm)

-- | Return all paths in an AVM
paths :: AVM -> [Path]
paths avm = go avm []
  where
    go :: AVM -> Path -> [Path]
    go avm pfx = map (\k -> pfx++[k]) (M.keys (avmBody avm)) ++ concatMap (uncurry f) (M.toList (avmBody avm))
      where
        f :: Attribute -> Value -> [Path]
        f a v = case v of
          ValAVM avm -> go avm (pfx++[a])
          ValIndex i -> case lookupAVM i avm of
            Just v -> f a v
            Nothing -> []
          _ -> []

-- | Are two paths reentrant in an AVM?
reentrant :: Path -> Path -> AVM -> Bool
reentrant p1 p2 avm = case (val p1 avm, val p2 avm) of
  (Just (ValIndex i1), Just (ValIndex i2)) -> i1 == i2
  _ -> False

-- | Is an AVM cyclic?
--   Ideally we should allow cycles, but makes thing loop infinitely
cyclic :: AVM -> Bool
cyclic avm = cyclicDict (avmDict avm)

-- | Is an dictionary cyclic?
--   Ideally we should allow cycles, but makes thing loop infinitely
cyclicDict :: Dict -> Bool
cyclicDict dict = or $ map (\(i,v) -> f [i] v) (M.toList (dict))
  where
    f :: [Index] -> Value -> Bool
    f is v = case v of
      ValAVM avm -> or $ map (\(_,v) -> f is v) (M.toList (avmBody avm))
      ValIndex i -> if i `elem` is then True else case lookupDict i dict  of
        Just v -> f (i:is) v
        Nothing -> False
      ValAtom _ -> False

-- | Is an AVM empty?
isnull :: AVM -> Bool
isnull avm = M.null (avmBody avm)

-- | Is an AVM inside a value empty?
visnull :: Value -> Bool
visnull (ValAVM avm) = isnull avm
visnull _ = False

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

-- | Add a dictionary to an AVM (overwrite)
addDict :: AVM -> [(Index,Value)] -> AVM
addDict avm d = avm { avmDict = M.fromList d }

-- | Add a dictionary to an ValAVM (overwrite)
vaddDict :: Value -> [(Index,Value)] -> Value
vaddDict (ValAVM avm) d = ValAVM $ avm { avmDict = M.fromList d }

-- | Make an AVM as a Value
vmkAVM :: AVList -> Value
vmkAVM = ValAVM . mkAVM

-- -- | Make an AVM with a dictionary as a Value
-- vmkAVM' :: AVList -> [(Index,Value)] -> Value
-- vmkAVM' l d = ValAVM $ mkAVM' l d

-- -- | Make an AVM with a name
-- mkAVMNamed :: Sort -> [(Attribute,Value)] -> AVM
-- mkAVMNamed name l = AVM (M.fromList ((Attr "SORT",ValAtom name):l)) M.empty

-- | Make a multi-AVM
--   Dictionaries from individual AVMs are merged into top
mkMultiAVM :: [Value] -> MultiAVM
mkMultiAVM vals | not $ all (\v -> isValAVM v || isValIndex v) vals = error $ "mkMultiAVM: invalid values: " ++ show vals
mkMultiAVM vals = MultiAVM body dict
  where
    avms :: [AVM] = map (\(ValAVM avm) -> avm) (filter isValAVM vals)
    body :: [Value] = map f vals
    f v = case v of
      ValAVM avm -> ValAVM (cleanDict avm)
      _ -> v
    dict = foldl mergeDicts M.empty (map avmDict avms)
-- mkMultiAVM :: [AVM] -> MultiAVM
-- mkMultiAVM avms = MultiAVM body dict
--   where
--     body = map (MAVM_AVMap . avmBody) avms
--     dict = foldl mergeDicts M.empty (map avmDict avms)

-- | Un-make a multi-AVM into a list of AVMs
unMultiAVM :: MultiAVM -> [AVM]
unMultiAVM mavm = map (toAVM mavm) [1..length (mavmBody mavm)]

-- | Add a dictionary to a MultiAVM
maddDict :: MultiAVM -> [(Index,Value)] -> MultiAVM
maddDict mavm d = mavm { mavmDict = M.fromList d }

------------------------------------------------------------------------------
-- Pretty print

-- | Pretty-print AVM with line-breaks and indentation
ppAVM :: AVM -> String
ppAVM avm = CMW.execWriter f
  where
    putStr s   = CMW.tell s
    putStrLn s = CMW.tell (s++"\n")

    f :: (CMW.MonadWriter String m) => m ()
    f = do
      go 0 (avmBody avm)
      CMW.tell "\n"
      ppDict (avmDict avm) False

    go :: (CMW.MonadWriter String m) => Int -> AVMap -> m ()
    go l av | M.null av = putStr "[]"
    go l av = do
      putStr $ "["
      mapM_ (uncurry $ f (l+1)) $ zip [0..] (M.toList av)
      where
        f :: (CMW.MonadWriter String m) => Int -> Int -> (Attribute,Value) -> m ()
        f l' i (Attr a,v) = do
          if i > 0
          then putStr $ replicate l' ' '
          else return ()
          putStr $ a++" "
          showValue v (\avm -> go (l'+(length a)+1) (avmBody avm))
          if i+1 == M.size av
          then putStr "]"
          else putStrLn " "

-- | Pretty-print a dictionary, either inline or not
ppDict :: (CMW.MonadWriter String m) => Dict -> Bool -> m ()
ppDict dict _ | M.size dict == 0 = return ()
ppDict dict inline = do
  let
    putStr s   = CMW.tell s
    putStrLn s = CMW.tell (s++"\n")
    dl = M.toList dict
    f (i,(x,val)) =
      if inline
      then do
        putStr $ show x ++ "="
        showValue val (CMW.tell . inlineAVM)
        if i < length dl then putStr ", " else return ()
      else do
        putStr $ " " ++ show x ++ ": "
        showValue val (CMW.tell . inlineAVM)
        if i < length dl then putStr "\n" else return ()
  if inline then putStr " where " else putStrLn "where"
  mapM_ f (zip [1..] dl)

-- | Pretty-print a MultiAVM
ppMAVM :: MultiAVM -> String
ppMAVM mavm = CMW.execWriter f
  where
    f :: (CMW.MonadWriter String m) => m ()
    f = do
      CMW.tell $ unlines $ map (ppAVM.cleanDict) (unMultiAVM mavm)
      ppDict (mavmDict mavm) False

-- | Helper function for showing values, recursively
--   `f` is the function applied to nested AVMs
showValue :: (CMW.MonadWriter String m) => Value -> (AVM -> m ()) -> m ()
showValue v f | islist v =
  do
    let vs = tolist v
    CMW.tell "<"
    if L.null vs
    then return ()
    else do
      showValue (head vs) f
      mapM_ (\v -> CMW.tell "," >> showValue v f) (tail vs)
    CMW.tell ">"
showValue v f =
  case v of
    ValAVM avm ->
      if M.size (avmDict avm) > 0
      then error "Non-empty middle dictionary"
      else f avm
    ValAtom s  -> CMW.tell s
    ValIndex i -> CMW.tell $ "#"++show i

-- | Pretty-print AVM in a single line
inlineAVM :: AVM -> String
inlineAVM avm = CMW.execWriter f
  where
    putStr s   = CMW.tell s

    f :: (CMW.MonadWriter String m) => m ()
    f = do
      go (avmBody avm)
      ppDict (avmDict avm) True

    go :: (CMW.MonadWriter String m) => AVMap -> m ()
    go av | M.null av = putStr "[]"
    go av = do
      putStr $ "["
      mapM_ (uncurry $ f) $ zip [0..] (M.toList av)
      where
        f :: (CMW.MonadWriter String m) => Int -> (Attribute,Value) -> m ()
        f i (Attr a,v) = do
          putStr $ a++" "
          showValue v (\avm -> go (avmBody avm))
          if i+1 == M.size av
          then putStr "]"
          else putStr ","

-- | Inline a MultiAVM (one per line)
inlineMAVM :: MultiAVM -> String
inlineMAVM mavm = CMW.execWriter f
  where
    f :: (CMW.MonadWriter String m) => m ()
    f = do
      CMW.tell $ unlines $ map (inlineAVM.cleanDict) (unMultiAVM mavm)
      ppDict (mavmDict mavm) False

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

(≐) = eq
infix 4 ≐ -- same as ==

-- | Equality that follows reentrants
(~=) = eq
infix 4 ~= -- same as ==

-- | Equality that follows reentrants
eq :: AVM -> AVM -> Bool
eq a b | (M.keys (avmBody a)) /= (M.keys (avmBody b)) = False
eq a b =
  all (\k -> M.member k b2 && eqV (b1 M.! k) (b2 M.! k)) (M.keys b1)
  where
    (b1,d1) = (avmBody a, avmDict a)
    (b2,d2) = (avmBody b, avmDict b)

    eqV :: Value -> Value -> Bool
    eqV (ValIndex i1) v2 | M.member i1 d1 = eqV (d1 M.! i1) v2
    eqV v1 (ValIndex i2) | M.member i2 d2 = eqV v1 (d2 M.! i2)
    eqV (ValIndex i1) (ValIndex i2) | not (M.member i1 d1) && not (M.member i2 d2) = True -- ignore actual indices when unbound

    eqV (ValAVM avm1) (ValAVM avm2) = (setDict d1 avm1) ~= (setDict d2 avm2)

    eqV v1 v2 = v1 == v2

------------------------------------------------------------------------------
-- Unification

-- | Unification
--   Throws an error if unification fails
(⊔) :: AVM -> AVM -> AVM
(⊔) a b = case unify a b of
  Left err -> error $ "unify: "++err
  Right avm -> avm
infixl 6 ⊔ -- same as +

-- | Unifiable
(⊔?) :: AVM -> AVM -> Bool
(⊔?) a b = case unify a b of
  Left _ -> False
  Right _ -> True
infix 4 ⊔? -- same as <

-- | Unification, with helpful error messages
unify :: AVM -> AVM -> Either String AVM
unify a1 a2 = do
  -- if not (distinctIndices a1 a2)
  -- then CME.throwError "I don't want to unify AVMs with intersecting indices"
  -- else return nullAVM -- dummy line

  dict <- mergeDictsM d1 d2
  (body, dict2) <- CMS.runStateT (unionWithM f b1 b2) dict
  return $ AVM body dict2

  -- TODO: cleanup: unreferred-to things in dict
  -- TODO: cleanup: pointers to pointers

  where
    -- a2' = replaceIndices (M.fromList $ zip (getIndices a2) [newIndex a1..]) a2
    AVM b1 d1 = a1
    AVM b2 d2 = a2 --a2'

    f :: (CME.MonadError String m, CMS.MonadState Dict m) => Value -> Value -> m Value

    -- Consider indices first
    f (ValIndex i1) (ValIndex i2) = do
      mv1 <- CMS.gets (lookupDict i1)
      mv2 <- CMS.gets (lookupDict i2)
      case (mv1,mv2) of
        (Just v1,Just v2) -> do
          v' <- f v1 v2
          CMS.modify (M.insert i1 v')
          -- CMS.modify (M.insert i2 (ValIndex i1))
          return $ ValIndex i1
        (Just v1,Nothing) -> do
          -- CMS.modify (M.insert i2 (ValIndex i1))
          return $ ValIndex i1
        (Nothing,Just v2) -> do
          -- CMS.modify (M.insert i1 (ValIndex i2))
          return $ ValIndex i2
        (Nothing,Nothing) -> do
          -- CMS.modify (M.insert i2 (ValIndex i1))
          return $ ValIndex i1
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

    -- General equality
    f v1 v2 | v1 == v2 = return v1
    f (ValAVM av1) (ValAVM av2)
      | (avmDict av1) /= M.empty = CME.throwError $ "Non-empty dictionary in value"
      | (avmDict av2) /= M.empty = CME.throwError $ "Non-empty dictionary in value"
      | otherwise = do avmap <- unionWithM f (avmBody av1) (avmBody av2)
                       return $ ValAVM $ AVM avmap M.empty
    f (ValAVM av1) v2 | isnull av1 = return v2
    f v1 (ValAVM av2) | isnull av2 = return v1

    -- This must match with treatment of nulls in subsumption
    f v1 v2 = CME.throwError $ printf "Cannot unify:\n  %s\n  %s" (show v1) (show v2)

    mergeDictsM :: (CME.MonadError String m) => Dict -> Dict -> m Dict
    mergeDictsM = unionWithKeyM f
      where
        f :: (CME.MonadError String m) => Index -> Value -> Value -> m Value
        f k v1 v2 = case mergeValues v1 v2 of
                      Just v -> return v
                      Nothing -> CME.throwError $ printf "Conflict for key %s: %s and %s " (show k) (show v1) (show v2) ++ "\n" ++ inlineAVM a1 ++ "\n" ++ inlineAVM a2

-- | Unification for multi-AVMs
munify :: MultiAVM -> MultiAVM -> Either String MultiAVM
munify a b | length (mavmBody a) /= length (mavmBody b) = Left "Multi-AVMs have different length"
           | otherwise = if length ls > 0
                         then Left  $ unlines ls
                         else Right $ mkMultiAVM (map ValAVM rs)
               where us = [ p `unify` q | (p,q) <- zip (unMultiAVM a) (unMultiAVM b) ] :: [Either String AVM]
                     (ls,rs) = partitionEithers us

------------------------------------------------------------------------------
-- Subsumption

-- | Are two AVMs comparable for subsumption ("subsumable")?
subsumable :: AVM -> AVM -> Bool
subsumable a b = a ⊑ b || a ⊒ b

-- | Subsumes
(⊑) :: AVM -> AVM -> Bool
(⊑) = subsumes
infix 4 ⊑ -- same as <

-- | Is subsumed by
(⊒) :: AVM -> AVM -> Bool
(⊒) = flip subsumes
infix 4 ⊒ -- same as >

-- | AVMs may subsume eachother
--   A subsumes B (A is more general than B)
--   B is subsumed by A (B is more specific than A)
subsumes :: AVM -> AVM -> Bool
subsumes a b =
  all (\k -> M.member k b2 && subV (b1 M.! k) (b2 M.! k)) (M.keys b1)
  && and [ if reentrant p1 p2 a then reentrant p1 p2 b else True | p1 <- paths a, p2 <- paths a, p1 /= p2 ]
  where
    (b1,d1) = (avmBody a, avmDict a)
    (b2,d2) = (avmBody b, avmDict b)

    subV :: Value -> Value -> Bool
    subV (ValIndex i1) (ValIndex i2) | not (M.member i1 d1) && not (M.member i2 d2) = True -- ignore actual indices when unbound
    subV (ValIndex i1) v2 | M.member i1 d1 = subV (d1 M.! i1) v2
                          | otherwise      = True
    subV v1 (ValIndex i2) | M.member i2 d2 = subV v1 (d2 M.! i2)
                          | otherwise      = False -- ?

    subV (ValAVM avm1) (ValAVM avm2) =
      let avm1' = AVM (avmBody avm1) d1
          avm2' = AVM (avmBody avm2) d2
      in subsumes avm1' avm2'

    -- This must match with treatment of nulls in unification
    subV (ValAVM avm) v2 | isnull avm = True -- converse not true

    subV v1 v2 = v1 == v2

-- | Subsumption for multi-AVMs
msubsumes :: MultiAVM -> MultiAVM -> Bool
msubsumes a b | length (mavmBody a) /= length (mavmBody b) = False
              | otherwise = and [ p `subsumes` q && reent p q | (p,q) <- zip (unMultiAVM a) (unMultiAVM b) ]
  where
    reent p q = and [ if reentrant pi1 pi2 p then reentrant pi1 pi2 q else True | pi1 <- paths p, pi2 <- paths p, pi1 /= pi2 ]

------------------------------------------------------------------------------
-- Generalisation

-- | Generalisation
(⊓) :: AVM -> AVM -> AVM
(⊓) = generalise
infixl 6 ⊓ -- same as +

-- | Generalisation
generalise :: AVM -> AVM -> AVM
generalise a1 a2 = AVM body dict
  where
    (b1,d1) = (avmBody a1, avmDict a1)
    (b2,d2) = (avmBody a2, avmDict a2)
    body = M.map fromJust $ M.filter isJust $ M.intersectionWithKey genV b1 b2
    dict = mergeDicts d1 d2 -- might throw error

    genV :: Attribute -> Value -> Value -> Maybe Value
    genV k (ValIndex i1) v2 | M.member i1 d1 = genV k (d1 M.! i1) v2
                            | otherwise      = Nothing
    genV k v1 (ValIndex i2) | M.member i2 d2 = genV k v1 (d2 M.! i2)
                            | otherwise      = Nothing
    genV k v1@(ValAVM avm) v2 | isnull avm = Just v1
    genV k v1 v2@(ValAVM avm) | isnull avm = Just v2
    genV k v1 v2 =
      if v1 == v2
      then Just v1
      else Nothing

------------------------------------------------------------------------------
-- Monadic Map functions

-- See: http://stackoverflow.com/a/19896320/98600
unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f mapA mapB =
  Tr.sequence $ M.unionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

unionWithKeyM :: (Monad m, Ord k) => (k -> a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithKeyM f mapA mapB =
  Tr.sequence $ M.unionWithKey (\k a b -> do {x <- a; y <- b; f k x y}) (M.map return mapA) (M.map return mapB)

intersectionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
intersectionWithM f mapA mapB =
  Tr.sequence $ M.intersectionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

intersectionWithKeyM :: (Monad m, Ord k) => (k -> a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
intersectionWithKeyM f mapA mapB =
  Tr.sequence $ M.intersectionWithKey (\k a b -> do {x <- a; y <- b; f k x y}) (M.map return mapA) (M.map return mapB)

mapWithKeyM :: (Monad m, Ord k) => (k -> a -> m a) -> M.Map k a -> m (M.Map k a)
mapWithKeyM f mapA =
  Tr.sequence $ M.mapWithKey (\k a -> do {x <- a; f k x}) (M.map return mapA)
