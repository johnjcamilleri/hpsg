-- | Just messing around
module Main where

-- import NLP.HPSG
import NLP.HPSG.AVM
import qualified Data.Map as M
import Data.Maybe

_9  = mkAVM [("NUM",ValAtom "sing")]
_10 = mkAVM [("PER",ValAtom "3rd")]

_11 = _9 ⊔ _10

_14 = _14a ⊔ _14b
_14a = mkAVM [
         ("A",ValAV (mkAV [("B",ValAtom "a")]))
       , ("C",ValAV (mkAV [("D",ValAV (mkAV [("B",ValAtom "a")]))]))
       ]
_14b = mkAVM [
         ("C",ValAV (mkAV [("D",ValAV (mkAV [("E",ValAtom "b")]))]))
       ]

_15 = _15b ⊔ _15a
_15a = (mkAVM [
         ("A",ValIndex 1)
       , ("C",ValAV (mkAV [("D",ValIndex 1)]))
       ]) {avmDict = M.fromList [(1,mkAV [("B",ValAtom "a")])]}
_15b = mkAVM [
         ("C",ValAV (mkAV [("D",ValAV (mkAV [("E",ValAtom "b")]))]))
       ]

main = do
  ppAVM _15a
  putStrLn ""
  ppAVM _15b
  putStrLn ""
  let Just avm = _15
  ppAVM avm
-- main = ppAVM _14
