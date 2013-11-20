-- | Feature structures.


module NLP.HPSG.FS
(
) where


import qualified Data.Set as S
import qualified Data.Map as M


-- -- IDEAS:
-- -- * We may also want to represent equality between FS substructures
-- --   by adding the (maybe) identifier to each feature value in the
-- --   `atts` map.
-- -- * We need to prepare a FS schema which described which feature
-- --   structures are valid and which are not.
-- 
-- 
-- -- | A feature structure parametrized over the type of the atomic
-- -- attributes and values.
-- data FS a = FS {
--     -- | Type of the feature structure.
--       ftyp  :: a
--     -- | Attributes and corresponding values.
--     , atts  :: M.Map a (FV a)
--     } deriving (Show, Eq, Ord)
-- 
-- 
-- -- | A feature value. 
-- data FV a
--     = Val a
--     | Set (S.Set a)
--     | Sub (FS a)


-- On the basis of the description in the book:
--
-- 1#
--
-- Types of feature structures are arranged in
-- a tree-like hierarchy.
--
-- It is certainly possible to represent this FS hierarchy on
-- the level of the Haskell type system, but is it reasonable?
-- No, it doesn't seems to be what we want!  It is more like
-- an inheritance hierarchy, which is not a kind of hierarchy
-- easily represented with the Haskell type system.  So there's
-- no point to do that.
--
-- 2#
--
-- It can be concluded from 3.3.4, that it should be
-- possible to define FS templates.


-- | Feature structure type.
data FSType a = FSType {
    -- | Name of the FS type.
      fsType    :: a
    -- | Map from attributes defined for a particular type to
    -- values (names of other FS types or atomic entities).
    -- Attribute names and value names can collide.
    , fsAtts    :: M.Map a (FVType a)
    } deriving (Show, Eq, Ord)


-- | An feature value.
data FVType a
    = Ptr a           -- ^ Pointer to another feature type
    | Dom (S.Set a)   -- ^ Set of possible values


---------------------------------------------------------------------
-- Hierarchy of types defined in the book
---------------------------------------------------------------------


-- | FS hierarchy smart constructor.
mkFS :: a -> [(a, a)] -> [FSType a] -> Tree (FSType a)
mkFS x ys = Node (FSType x $ M.fromList ys)


-- | Domain smart constructor.
mkDom :: [a] -> FVType a
mkDom xs = FVType . S.fromList


-- | Hierarchy of FS types.
typHier :: Tree (FSType String)
typHier = mkFS "feat-struc" []
    [ mkFS "expression" [("head", Ptr "pos")]
        [ mkFS "phrase" []
        , mkFS "word" [] ]
    , mkFS "pos" []
        [ mkFS "agr-pos" [("agr", ?)]
            [ mkFS "noun" []
            , mkFS "verb" [("aux", ?)]
            , mkFS "det" [] ]
        , mkFS "prep"
        , mkFS "adj"
        , mkFS "conj" ]
    , mkFS "val-cat" [("comps", mkDom ["itr", "str", "dtr"])]
