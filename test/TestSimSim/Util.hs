{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
--     Update #: 28
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
    , (====)
    , eqPretty
    ) where

import           ClassyPrelude                as P
import           Data.Algorithm.Diff
import           Data.Algorithm.DiffOutput
import           Test.QuickCheck              hiding ((===))
import qualified Test.QuickCheck              as QC
import           Text.PrettyPrint.ANSI.Leijen


-- | Like '==', but prints a counterexample when it fails.
infix 4 ====
(====) :: (Show a, Eq a) => a -> a -> Property
(====) = (QC.===)


-- | Like '==', but prints a counterexample when it fails.
infix 4 ===
(===) :: (Pretty a, Eq a) => a -> a -> Property
x === y = eqPretty x pretty y pretty


-- | Like '==', but prints a counterexample when it fails.
eqPretty :: (Pretty a, Eq a) => a -> (a -> Doc) -> a -> (a -> Doc) -> Property
eqPretty x xDocF y yDocF =
  counterexample (show xDoc ++ interpret res ++ show yDoc ++ "\n\tDiff: " ++ ppDiff diffs) res
  where
    res = x P.== y
    xDoc = xDocF x
    yDoc = yDocF y
    diffs = getGroupedDiffBy (P.==) (lines $ show xDoc) (lines $ show yDoc)
    interpret True  = " == "
    interpret False = " /= "


--
-- Util.hs ends here
