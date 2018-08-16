-- Util.hs ---
--
-- Filename: Util.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug 16 10:18:24 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 20
-- URL:
-- Doc URL:
-- Keywords:
-- Compatibility:
--
--

-- Commentary:
--
--
--
--

-- Change Log:
--
--
--
--
--
--
--

-- Code:

module TestSimSim.Util
    ( (===)
    ) where

import           ClassyPrelude                as P
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Test.QuickCheck              hiding ((===))
import           Text.PrettyPrint.ANSI.Leijen

(====) :: (Pretty a, Eq a) => a -> a -> Property
l ==== r = whenFail (print err) (l P.== r)
  where err = pretty l ++ "\n\n/=\n\n" ++ pretty r


-- | Like '==', but prints a counterexample when it fails.
infix 4 ===
(===) :: (Pretty a, Eq a) => a -> a -> Property
x === y =
  counterexample (show' x ++ interpret res ++ show' y ++ "\n\tDiff: " ++ ppDiff diffs) res
  where
    res = x P.== y
    diffs = getGroupedDiffBy (P.==) (lines $ show' x) (lines $ show' y)
    interpret True  = " == "
    interpret False = " /= "
    show' = show . pretty

--
-- Util.hs ends here
