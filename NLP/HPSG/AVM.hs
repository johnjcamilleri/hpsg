module NLP.HPSG.AVM where

import qualified Data.Map as M
import Data.Maybe

-- | Attribute or feature, often drawn in CAPS
data Attribute = Attr String
  deriving (Eq, Ord, Show)

-- | Value of an attribute
data Value = ValAVM AVM      -- ^ A sub-structure
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
  avmBody :: M.Map Attribute Value,
  -- ^ Dictionary used for structure sharing
  avmDict :: M.Map Index AV
  }
  deriving (Eq, Ord, Show)

-- | A map of attribute-values
data AV = AV (M.Map Attribute Value)
  deriving (Eq, Ord, Show)

-- | Make an AVM with no name
mkAVM :: [(Attribute,Value)] -> AVM
mkAVM l = AVM (M.fromList l) M.empty

-- | Make an AVM with a name
mkAVMNamed :: Sort -> [(Attribute,Value)] -> AVM
mkAVMNamed name l = AVM (M.fromList ((Attr "SORT",ValAtom name):l)) M.empty

ppAVM :: AVM -> IO ()
ppAVM avm = go 0 avm
  where
    go l (AVM b d) = do
      case M.lookup (Attr "SORT") b of
        Just (ValAtom s) -> putStrLn $ (replicate l ' ') ++ ("[" ++ s)
        _ -> return ()
      mapM_ (uncurry f) (M.toList b)
      where
        f (Attr a) v = do
          putStr (replicate l ' ')
          putStr $ "["++a++" "
          case v of
            ValAVM avm -> putStr "\n" >> go (l+(length a)+1) avm
            ValAtom s  -> putStrLn $ s
            ValList vs -> putStrLn $ "<"++replicate (length vs) '.'++">"
            ValIndex i -> putStrLn $ show i

(⊔) :: AVM -> AVM -> Maybe AVM
(⊔) = unify

-- | Unification
unify :: AVM -> AVM -> Maybe AVM
unify a1 a2 = Just $ AVM body dict
  where
    body = M.unionWith f (avmBody a1) (avmBody a2)
    dict = M.union (avmDict a1) (avmDict a2)
    f :: Value -> Value -> Value
    f (ValAVM avm1) (ValAVM avm2) = ValAVM $ fromJust $ unify avm1 avm2
    f (ValAtom a1) (ValAtom a2) | a1 == a2  = ValAtom a1
                                | otherwise = error "Cannot unify"
    f (ValList l1) (ValList l2) = ValList (l1++l2)
    f (ValIndex i1) v2 = undefined

    -- f (ValIndex i1) (ValIndex i2) | i1 == i2  = ValIndex i1
    --                               | otherwise = error "Cannot unify"
    f v1 v2 = error $ "Cannot unify:\n  "++show v1++"\n  "++show v2

-- | AVMs may subsume eachother
subsumes :: AVM -> AVM -> Ordering
subsumes = undefined
