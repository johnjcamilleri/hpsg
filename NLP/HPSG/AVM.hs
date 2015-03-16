{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module NLP.HPSG.AVM where

import qualified Data.Map as M
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Except as CME
import Data.Maybe
import Text.Printf (printf)

import qualified Data.Traversable as Tr

------------------------------------------------------------------------------
-- Types

-- | Attribute or feature, often drawn in CAPS
data Attribute = Attr String
  deriving (Eq, Ord, Show)

-- | Value of an attribute
data Value = -- ValAV AV        -- ^ A sub-structure
             ValAVM AVM      -- ^ A sub-structure (with its own dictionary - should always be factored up)
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
-- Builders

type AVList = [(String,Value)]

attrList :: [(String,v)] -> [(Attribute,v)]
attrList = map (\(a,v)->(Attr a,v))

attrMap :: [(String,v)] -> M.Map Attribute v
attrMap = M.fromList . attrList

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

showValue :: Value -> String
showValue v = case v of
  ValAVM avm ->
    if M.size (avmDict avm) > 0
    then error "Non-empty middle dictionary"
    else "[...]" -- TODO
  ValAtom s  -> s
  ValList vs -> "<"++replicate (length vs) '.'++">"
  ValIndex i -> show i
  ValNull -> "[]"

ppAVM :: AVM -> IO ()
ppAVM avm = do
  go 0 (avmBody avm)
  putStr "\n"
  if M.size (avmDict avm) > 0
  then do
    putStrLn "where"
    mapM_ (\(i,val) -> putStrLn $ show i ++ ": " ++ showValue val ++ "\n") (M.toList $ avmDict avm)
  else return ()
  where
    go :: Int -> AVMap -> IO ()
    go l av = do
      -- putStr $ replicate l ' '
      putStr $ "["
      -- case M.lookup (Attr "SORT") (avmBody avm) of
      --   Just (ValAtom s) -> putStrLn $ (replicate l ' ') ++ ("[" ++ s)
      --   _ -> return ()
      mapM_ (uncurry $ f (l+1)) $ zip [0..] (M.toList av)
      where
        f l' i ((Attr a),v) = do
          if i > 0
          then putStr $ replicate l' ' '
          else return ()
          putStr $ a++":"
          case v of
            ValAVM avm ->
              if M.size (avmDict avm) > 0
              then error "Non-empty middle dictionary"
              else go (l'+(length a)+1) (avmBody avm)
            ValAtom s  -> putStr $ s
            ValList vs -> putStr $ "<"++replicate (length vs) '.'++">"
            ValIndex i -> putStr $ show i
            ValNull    -> putStr "[]"
          if i+1 == M.size av
          then putStr "]"
          else putStrLn " "

------------------------------------------------------------------------------
-- Unification

-- | Unification.
--   Throws an error if unification fails
(&) :: AVM -> AVM -> AVM
(&) a b = case unify a b of
  Left err -> error err
  Right avm -> avm

addToDict :: (CME.MonadError String m, CMS.MonadState Dict m) => Dict -> m ()
addToDict dict = CMS.modify (dictMerge dict)

dictMerge :: Dict -> Dict -> Dict
dictMerge = M.unionWithKey (\k _ -> error $ "Conflicting key: "++show k)

-- | Unification, with helpful error messages
unify :: AVM -> AVM -> Either String AVM
unify a1 a2 = do
  let
    dict = dictMerge (avmDict a1) (avmDict a2)
  (body', dict') <- CMS.runStateT (s a1 a2) dict
  return $ AVM body' dict'

  where
    s :: (CME.MonadError String m, CMS.MonadState Dict m) => AVM -> AVM -> m AVMap
    s a1 a2 = unionWithM f (avmBody a1) (avmBody a2)

    f :: (CME.MonadError String m, CMS.MonadState Dict m) => Value -> Value -> m Value
    f (ValAVM av1) (ValAVM av2) = do
      addToDict (avmDict av1)
      addToDict (avmDict av2)
      avmap <- unionWithM f (avmBody av1) (avmBody av2)
      return $ ValAVM $ AVM avmap M.empty
    f (ValAVM av1) v2 | is_empty av1 = return v2
    f v1 (ValAVM av2) | is_empty av2 = return v1
    f (ValAtom a1) (ValAtom a2) | a1 == a2  = return $ ValAtom a1
                                | otherwise = CME.throwError $ printf "Cannot unify: %s and %s" a1 a2
    f (ValList l1) (ValList l2) = return $ ValList (l1++l2)
    f (ValIndex i1) v2 = do
      v1 <- CMS.gets (\m -> m M.! i1)
      ValAVM v1' <- f v1 v2
      -- CMS.modify (M.insert i1 v1')
      return $ ValIndex i1
    f v1 (ValIndex i2) = do
      v2 <- CMS.gets (\m -> m M.! i2)
      ValAVM v2' <- f v1 v2
    --   CMS.modify (M.insert i2 v2')
      return $ ValIndex i2
    f ValNull v2 = return v2
    f v1 ValNull = return v1
    f v1 v2 = CME.throwError $ printf "Cannot unify:\n  %s\n  %s" (show v1) (show v2)

    is_empty :: AVM -> Bool
    is_empty = (== M.empty) . avmBody

-- See: http://stackoverflow.com/a/19896320/98600
unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f mapA mapB =
  Tr.sequence $ M.unionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

------------------------------------------------------------------------------
-- Subsumption

-- -- | AVMs may subsume eachother
-- subsumes :: AVM -> AVM -> Ordering
-- subsumes = undefined
