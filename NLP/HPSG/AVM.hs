module NLP.HPSG.AVM where

import qualified Data.Map as M
import qualified Control.Monad.State as CMS
import Data.Maybe
import Text.Printf (printf)

import qualified Data.Traversable as Tr

-- | Attribute or feature, often drawn in CAPS
data Attribute = Attr String
  deriving (Eq, Ord, Show)

-- | Value of an attribute
data Value = ValAV AV        -- ^ A sub-structure
           | ValAtom Sort    -- ^ Atomic value
           | ValList [Value] -- ^ List of values
           | ValIndex Index  -- ^ Index to structure in dict
  deriving (Eq, Ord, Show)

-- | Used for structure sharing
type Index = Int

-- | Type of an AVM
type Sort = String

-- | An attribute-value matrix, with bound variables
data AVM = AVM {
  -- ^ The inner AVM
  avmBody :: AV,
  -- ^ Dictionary used for structure sharing
  avmDict :: M.Map Index AV
  }
  deriving (Eq, Ord, Show)

-- | A map of attribute-values
type AV = M.Map Attribute Value

attrList :: [(String,v)] -> [(Attribute,v)]
attrList = map (\(a,v)->(Attr a,v))

attrMap :: [(String,v)] -> M.Map Attribute v
attrMap = M.fromList . attrList

-- | Make an AV
mkAV :: [(String,Value)] -> AV
mkAV = attrMap

vmkAV = ValAV . mkAV

-- | Make an AV with a single attribute
mkAV1 :: String -> Value -> AV
mkAV1 a v = mkAV [(a,v)]

vmkAV1 a v = ValAV $ mkAV1 a v

-- | Make an AVM with no name
mkAVM :: [(String,Value)] -> AVM
mkAVM l = AVM (attrMap l) M.empty

-- -- | Make an AVM with a name
-- mkAVMNamed :: Sort -> [(Attribute,Value)] -> AVM
-- mkAVMNamed name l = AVM (M.fromList ((Attr "SORT",ValAtom name):l)) M.empty

ppAVM :: AVM -> IO ()
ppAVM avm = do
  go 0 (avmBody avm)
  putStr "\n"
  if M.size (avmDict avm) > 0
  then do
    putStrLn "where"
    mapM_ (\(i,av) -> putStrLn (show i++": ") >> go 0 av >> putStr "\n") (M.toList $ avmDict avm)
  else return ()
  where
    go l av = do
      -- putStr $ replicate l ' '
      putStr $ "["
      -- case M.lookup (Attr "SORT") av of
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
            ValAV  av  -> go (l'+(length a)+1) av
            ValAtom s  -> putStr $ s
            ValList vs -> putStr $ "<"++replicate (length vs) '.'++">"
            ValIndex i -> putStr $ show i
          if i+1 == M.size av
          then putStr "]"
          else putStrLn " "

(⊔) :: AVM -> AVM -> Maybe AVM
(⊔) = unify

-- | Unification
unify :: AVM -> AVM -> Maybe AVM
unify a1 a2 = Just $ AVM body' dict'
  where
    dict = M.unionWithKey (\k _ -> error $ "Conflicting key: "++show k) (avmDict a1) (avmDict a2)
    (body',dict') = CMS.runState s (dict)

    s :: CMS.State (M.Map Index AV) AV
    s = unionWithM f (avmBody a1) (avmBody a2)

    f :: Value -> Value -> CMS.State (M.Map Index AV) Value
    f (ValAV av1) (ValAV av2) = unionWithM f av1 av2 >>= \av -> return $ ValAV av
    f (ValAtom a1) (ValAtom a2) | a1 == a2  = return $ ValAtom a1
                                | otherwise = error $ printf "Cannot unify: %s and %s" a1 a2
    f (ValList l1) (ValList l2) = return $ ValList (l1++l2)
    f (ValIndex i1) v2 = do
      v1 <- CMS.gets (\m -> m M.! i1)
      ValAV v1' <- f (ValAV v1) v2 -- TODO
      CMS.modify (\m -> M.insert i1 v1' m)
      return $ ValIndex i1
    f v1 (ValIndex i2) = do
      v2 <- CMS.gets (\m -> m M.! i2)
      ValAV v2' <- f v1 (ValAV v2) -- TODO
      CMS.modify (\m -> M.insert i2 v2' m)
      return $ ValIndex i2
    f v1 v2 = error $ printf "Cannot unify:\n  %s\n  %s" (show v1) (show v2)

-- See: http://stackoverflow.com/a/19896320/98600
unionWithM :: (Monad m, Ord k) => (a -> a -> m a) -> M.Map k a -> M.Map k a -> m (M.Map k a)
unionWithM f mapA mapB =
  Tr.sequence $ M.unionWith (\a b -> do {x <- a; y <- b; f x y}) (M.map return mapA) (M.map return mapB)

-- | AVMs may subsume eachother
subsumes :: AVM -> AVM -> Ordering
subsumes = undefined
