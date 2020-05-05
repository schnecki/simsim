{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
--     Update #: 9
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
  arbitrary = Shipment <$> arbitrary <*> arbitrary <*> (pack <$> arbitrary)

instance CoArbitrary Shipment where
  coarbitrary (Shipment s a b) = variant 0 . coarbitrary (s, a, unpack b)

instance Arbitrary ShipmentRegularity where
  arbitrary = oneof $ map return [ShipEndOfPeriod, ShipWhenStoppedAndEndOfPeriod]

instance CoArbitrary ShipmentRegularity where
  coarbitrary ShipEndOfPeriod               = variant 0
  coarbitrary ShipWhenStoppedAndEndOfPeriod = variant 1

--
-- Instances.hs ends here
