-- | Just messing around
module Main where

-- import NLP.HPSG
import NLP.HPSG.AVM
import qualified Data.Map as M
import Data.Maybe

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

_15 = _15a ⊔ _15b
_15a = (mkAVM [
         (Attr "A",ValIndex 1)
       , (Attr "C",ValAVM (mkAVM [(Attr "D",ValIndex 1)]))
       ]) {avmDict = M.fromList [(1,AV (M.fromList [(Attr "B",ValAtom "a")]))]}
_15b = mkAVM [
         (Attr "C",ValAVM (mkAVM [(Attr "D",ValAVM (mkAVM [(Attr "E",ValAtom "b")]))]))
       ]

main = let Just avm = _14 in ppAVM avm
-- main = ppAVM _14
