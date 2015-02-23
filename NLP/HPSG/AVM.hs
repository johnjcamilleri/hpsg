module NLP.HPSG.AVM where

import qualified Data.Map as M

data Attribute = Attr String
  deriving (Eq, Ord, Show)

data Value = ValAVM AVM
           | ValAtom Sort
           | ValList [Value]
           | ValIndex Index
  deriving (Eq, Ord, Show)

type Index = Int

type Sort = String

data AVM = AVM {
  avmSort :: Maybe Sort,
  avmBody :: M.Map Attribute Value
  }
  deriving (Eq, Ord, Show)

mkAVM :: [(Attribute,Value)] -> AVM
mkAVM l = AVM Nothing (M.fromList l)

(⊔) :: AVM -> AVM -> Maybe AVM
(⊔) = unify

unify :: AVM -> AVM -> Maybe AVM
unify a b = Just a

subsumes :: AVM -> AVM -> Ordering
subsumes = undefined
