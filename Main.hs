-- | Just messing around
module Main where

-- import NLP.HPSG
import NLP.HPSG.AVM
import qualified Data.Map as M

_9  = mkAVM [(Attr "NUM",ValAtom "sing")]
_10 = mkAVM [(Attr "PER",ValAtom "3rd")]

_11 = _9 ⊔ _10

_14 = _14a ⊔ _14b
_14a = mkAVM [
         (Attr "A",ValAVM (mkAVM [(Attr "B",ValAtom "a")]))
       , (Attr "C",ValAVM (mkAVM [(Attr "D",ValAVM (mkAVM [(Attr "B",ValAtom "a")]))]))
       ]
_14b = mkAVM [
         (Attr "C",ValAVM (mkAVM [(Attr "D",ValAVM (mkAVM [(Attr "E",ValAtom "b")]))]))
       ]

-- walks :: BFS String
-- walks = BFS fs M.empty
--   where
--     fs = M.fromList [("word", (Left (Val "word")))]

main = print _11
