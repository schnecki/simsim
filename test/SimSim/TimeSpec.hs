-- TimeSpec.hs ---
--
-- Filename: TimeSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  8 12:10:21 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 2
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

module SimSim.TimeSpec (spec) where

import           ClassyPrelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Time

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary
instance CoArbitrary Time where
  coarbitrary (Time t) = variant 0 . coarbitrary t


spec :: Spec
spec = return ()


--
-- TimeSpec.hs ends here
