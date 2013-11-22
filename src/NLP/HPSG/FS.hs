-- | Feature structures.


module NLP.HPSG.FS
(
-- * Feature structure
  FS
, BFS (..)
, FV
-- * Unification
-- , unifyWrong
) where


-- import           Control.Applicative ((<$>))
-- import           Data.Traversable (traverse)
import qualified Data.Map as M


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
-- It can be concluded from 3.3.4, that it should be possible
-- to define FS templates (abbreviated descriptions, in other words).
-- Templates in 3.3.4 are simple, but perhaps it makes sense to allow
-- functional (lambda) templates as well.


-- -- | Feature structure type.
-- data FSType a = FSType {
--     -- | Name of the FS type.
--       fsType    :: a
--     -- | Map from attributes defined for a particular type to
--     -- values (names of other FS types or atomic entities).
--     -- Attribute names and value names can collide.
--     , fsAtts    :: M.Map a (FVType a)
--     } deriving (Show, Eq, Ord)
-- 
-- 
-- -- | A feature value.
-- data FVType a
--     = Ptr a           -- ^ Pointer to another feature type
--     | Dom (S.Set a)   -- ^ Set of possible values
-- 
-- 
-- ---------------------------------------------------------------------
-- -- Hierarchy of types defined in the book
-- ---------------------------------------------------------------------
-- 
-- 
-- -- | FS hierarchy smart constructor.
-- mkFS :: a -> [(a, a)] -> [FSType a] -> Tree (FSType a)
-- mkFS x ys = Node (FSType x $ M.fromList ys)
-- 
-- 
-- -- | Domain smart constructor.
-- dom :: [a] -> FVType a
-- dom xs = FVType . S.fromList
-- 
-- 
-- -- | Hierarchy of FS types.
-- typHier :: Tree (FSType String)
-- typHier = mkFS "feat-struc" []
--     [ mkFS "expression"
--         [ ("head", Ptr "pos")
--         , ("val", Ptr "val-cat") ]
--         [ mkFS "phrase" []
--         , mkFS "word" [] ]
--     , mkFS "pos" []
--         [ mkFS "agr-pos" [("agr", ?)]
--             [ mkFS "noun" []
--             , mkFS "verb" [("aux", ?)]
--             , mkFS "det" [] ]
--         , mkFS "prep"
--         , mkFS "adj"
--         , mkFS "conj" ]
--     , mkFS "val-cat"
--         [ ("comps", dom ["itr", "str", "dtr"])
--         , ("spr", dom ["-", "+"]) ]


---------------------------------------------------------------------
-- Feature structures
---------------------------------------------------------------------


-- | A feature structure with bound variables.
-- TODO: Do we assume, that every variable is bound, or (more
-- naturally), that some of the variables are bound?
data BFS a = BFS {
    -- | Feature structure with free variables.
      freeFS :: FS a
    -- | A map from variable names to feature values.  Be aware,
    -- that values in the map may contain variable references as
    -- well, thus introducing inter-dependencies between individual
    -- variables.
    , valMap :: M.Map a (FV a)
    } deriving (Show, Eq, Ord)


-- BUT: The feature values contained in the BFS structure can
-- include variables as well!  What conditions must be satisfied
-- by such a structure?  Do internal variables have to be assigned
-- by BFS as well?  Yes, it seems the only reasonable solution.


-- | A feature structure, possibly with variables.  Traditionally,
-- the feature type is represented separately and it has always
-- an atomic value, but there seems to be no point in enforcing
-- this on the level of the type system.
type FS a = M.Map a (Either (FV a) a)


-- | A feature value.
data FV a
    = Sub (FS a)        -- ^ Feature sub-structure
    | Val a             -- ^ Atomic value
    deriving (Show, Eq, Ord)


---------------------------------------------------------------------
-- Simple (and wrong) unification
---------------------------------------------------------------------


-- -- | Disregard variables and perform simplified unification of the
-- -- two given feature structures.
-- unifyWrong :: Ord a => FS a -> FS a -> Maybe (FS a)
-- unifyWrong s1 s2 = do
--     mut <- traverse id $ M.intersectionWith unifyFN s1 s2
--     return $ mut `M.union` s1 -|- s2
--   where
--     unifyFN (FN q x) (FN _ y) = FN q <$> unifyFV x y
--     unifyFV (Val x) (Val y) = if x == y
--         then Just $ Val x
--         else Nothing
--     unifyFV (Sub x) (Sub y) = Sub <$> unifyWrong x y
--     unifyFV _ _ = Nothing


---------------------------------------------------------------------
-- Notes on parsing and unification
---------------------------------------------------------------------


-- | The question: what unification capabilities are we going to
-- need within the context of parsing?
--
-- The basic parsing step consists of matching a particular rule to a
-- (contiguous?) sequence of "expressions" (words or phrases).  At this
-- point we have to perform unification between right-hand side elements
-- of the rule and corresponding expressions.  Constituents produced
-- during the parsing stage are fully specified, so it seems that we
-- don't even need to be able to perform unification between two
-- variable-adorned feature structures: one of them will always be fully
-- specified (i.e., without variables).
--
-- What we need to be able to do is to perform unification between
-- a sequence of feature structures (possibly with internal references
-- between individual components) and a sequence of fully-specified
-- constituents.
--
-- It can be also seen a little differently: first we match each
-- (FS, constituent) pair separately, and only afterwards we perform
-- unification between variables which occur in different FSs.
--
-- Once the match is "assured", we have to rewrite values assigned
-- to individual variables (determined during earlier steps), but
-- it seems to be a simple task.


---------------------------------------------------------------------
-- Misc
---------------------------------------------------------------------


-- | Symmetric difference on maps.
(-|-) :: Ord a => M.Map a b -> M.Map a b -> M.Map a b
(-|-) x y = (x M.\\ y) `M.union` (y M.\\ x)
