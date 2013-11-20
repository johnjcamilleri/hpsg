-- | Feature structures.


module NLP.HPSG.FS
(
) where


-- IDEAS:
-- * We may also want to represent equality between FS substructures
--   by adding the (maybe) identifier to each feature value in the
--   `atts` map.
-- * We need to prepare a FS schema which described which feature
--   structures are valid and which are not.


-- | A feature structure parametrized over the type of the atomic
-- attributes and values.
data FS a = FS {
    -- | Type of the feature structure.
      ftyp  :: a
    -- | Attributes and corresponding values.
    , atts  :: M.Map a (FV a)
    } deriving (Show, Eq, Ord)


-- | A feature value. 
data FV a
    = Val a
    | Set (S.Set a)
    | Sub (FS a)
