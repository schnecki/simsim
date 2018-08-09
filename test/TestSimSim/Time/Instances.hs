-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:50:39 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 1
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


module TestSimSim.Time.Instances where

import           ClassyPrelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Time

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary
instance CoArbitrary Time where
  coarbitrary (Time t) = variant 0 . coarbitrary t


--
-- Instances.hs ends here
