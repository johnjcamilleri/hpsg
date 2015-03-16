-- | Just messing around
module Main where

-- import NLP.HPSG
import NLP.HPSG.AVM
-- import NLP.HPSG.Templates

import qualified Data.Map as M
import Data.Maybe

------------------------------------------------------------------------------
-- Examples from:

-- _9  = mkAVM [("NUM",ValAtom "sing")]
-- _10 = mkAVM [("PER",ValAtom "3rd")]

-- _11 = _9 & _10

-- _14 = _14a & _14b
-- _14a = mkAVM [
--          ("A",ValAV (mkAV [("B",ValAtom "a")]))
--        , ("C",ValAV (mkAV [("D",ValAV (mkAV [("B",ValAtom "a")]))]))
--        ]
-- _14b = mkAVM [
--          ("C",ValAV (mkAV [("D",ValAV (mkAV [("E",ValAtom "b")]))]))
--        ]

-- _15 = _15b & _15a
-- _15a = (mkAVM [
--          ("A",ValIndex 1)
--        , ("C",ValAV (mkAV [("D",ValIndex 1)]))
--        ]) {avmDict = M.fromList [(1,mkAV [("B",ValAtom "a")])]}
-- _15b = mkAVM [
--          ("C",ValAV (mkAV [("D",ValAV (mkAV [("E",ValAtom "b")]))]))
--        ]

------------------------------------------------------------------------------
-- Examples from: http://www3.cs.stonybrook.edu/~ychoi/cse507/slides/08-unification.pdf

sg = mkAVM [("NUMBER",ValAtom "sg")]
pl = mkAVM [("NUMBER",ValAtom "pl")]
nonum = mkAVM [("NUMBER",ValNull)]
p3 = mkAVM [("PERSON",ValAtom "p3")]

num_sg_per_p3 = vmkAVM [("NUMBER",ValAtom "sg"),("PERSON",ValAtom "p3")]
num_pl_per_p3 = vmkAVM [("NUMBER",ValAtom "pl"),("PERSON",ValAtom "p3")]

eg1a = mkAVM'
       [ ("AGREEMENT",ValIndex 1)
       , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (ValIndex 1)) ]
       [ (1, num_sg_per_p3) ]
eg1b = mkAVM1 "SUBJECT" $ vmkAVM1 "AGREEMENT" num_sg_per_p3

eg2a = mkAVM
       [ ("AGREEMENT",ValIndex 1)
       , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (ValIndex 1)) ]
eg2b = mkAVM1 "SUBJECT" $ vmkAVM1 "AGREEMENT" num_sg_per_p3

eg3a = mkAVM'
       [ ("AGREEMENT",ValIndex 1)
       , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (ValIndex 1)) ]
       [ (1, num_sg_per_p3) ]
eg3b = mkAVM
       [ ("AGREEMENT",num_sg_per_p3)
       , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" num_pl_per_p3) ]

eg4a = mkAVM
       [ ("AGREEMENT",vmkAVM1 "NUMBER" (ValAtom "sg"))
       , ("SUBJECT"  ,vmkAVM1 "AGREEMENT" (vmkAVM1 "NUMBER" (ValAtom "sg"))) ]
eg4b = mkAVM1 "SUBJECT" (vmkAVM1 "AGREEMENT" num_sg_per_p3)

main = do
  -- putStrLn $ ppAVM (sg & sg)
  -- putStrLn $ ppAVM (sg & pl) -- fails
  -- putStrLn $ ppAVM (sg & nonum)
  -- putStrLn $ ppAVM (sg & p3)
  -- putStrLn $ ppAVM (eg1a & eg1b)
  -- putStrLn $ ppAVM (eg2a & eg2b)
  -- putStrLn $ ppAVM (eg3a & eg3b) -- fails
  putStrLn $ ppAVM (eg4a & eg4b)

------------------------------------------------------------------------------
-- Examples from: https://web.cs.dal.ca/~vlado/stefsexamples/04.html

-- // Lexical entries
-- [cat:[head:[noun]
--       subj:-     ]] -> sandy.   // Now "sandy" cannot have a subject

-- [cat:[head:[verb]
--       subj:cat:head:[noun]]] -> snored.

-- // Grammar rules
-- [cat:head:[verb]] -> [cat:head:#Head] [cat:subj:cat:head:#Head].

-- > parse "sandy snored"
-- [top
--  cat: [top
--        head: [verb]]
-- _from: 0
--   _to: 12
-- _orth: 'sandy snored']
