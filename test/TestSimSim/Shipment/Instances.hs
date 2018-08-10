-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Aug 10 19:51:45 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 7
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

module TestSimSim.Shipment.Instances where

import           ClassyPrelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Shipment.Type

import           TestSimSim.Block.Instances
import           TestSimSim.Order.Instances
import           TestSimSim.Time.Instances


instance Arbitrary Shipment where
  arbitrary = Shipment <$> arbitrary <*> (pack <$> arbitrary)

instance CoArbitrary Shipment where
  coarbitrary (Shipment a b) = variant 0 . coarbitrary (a, unpack b)


--
-- Instances.hs ends here
