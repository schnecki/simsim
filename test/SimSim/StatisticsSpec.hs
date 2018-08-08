-- StatisticsSpec.hs ---
--
-- Filename: StatisticsSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  8 09:34:52 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Wed Aug  8 12:29:14 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 57
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

module SimSim.StatisticsSpec (spec) where

import qualified Data.Map.Strict        as M
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Order
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Type

import           SimSim.BlockSpec       hiding (spec)
import           SimSim.OrderSpec       hiding (spec)

maxVal :: Num a => a
maxVal = 5000

instance Arbitrary StatsBlockTime where
  arbitrary = StatsBlockTime <$> choose (0, maxVal)
instance CoArbitrary StatsBlockTime where
  coarbitrary (StatsBlockTime pr) = variant 0 . coarbitrary pr

instance Arbitrary StatsOrderCost where
  arbitrary = StatsOrderCost <$> choose (0, maxVal) <*> choose (0, maxVal) <*> choose (0, maxVal) <*> choose (0, maxVal)
instance CoArbitrary StatsOrderCost where
  coarbitrary (StatsOrderCost ear wip bo fgi) = variant 0 . coarbitrary (ear, wip, bo, fgi)

instance Arbitrary StatsOrderTime where
  arbitrary = StatsOrderTime <$> choose (0, maxVal) <*> choose (0, maxVal)
instance CoArbitrary StatsOrderTime where
  coarbitrary (StatsOrderTime sumT stdDevT) = variant 0 . coarbitrary (sumT, stdDevT)

instance Arbitrary StatsOrderTard where
  arbitrary = StatsOrderTard <$> choose (0, maxVal) <*> choose (0, maxVal) <*> choose (0, maxVal)
instance CoArbitrary StatsOrderTard where
  coarbitrary (StatsOrderTard nr s stdDev) = variant 0 . coarbitrary (nr, s, stdDev)

instance Arbitrary SimStats where
  arbitrary = SimStats <$> arbitrary <*> arbitrary <*> arbitrary
instance CoArbitrary SimStats where
  coarbitrary (SimStats nr ft tard) = variant 0 . coarbitrary (nr, ft, tard)

instance Arbitrary SimStatistics where
  arbitrary =
    sized $ \n -> do
      let blocks = sizedBlocks n
      simStats <- take (length blocks) <$> arbitrary
      times <- take (length blocks) <$> arbitrary
      SimStatistics (M.fromList $ zip blocks simStats) (M.fromList $ zip blocks times) <$> arbitrary <*> arbitrary <*>
        arbitrary
instance CoArbitrary SimStatistics where
  coarbitrary (SimStatistics bl blTimes fl flFgi csts) = variant 0 . coarbitrary (bl, blTimes, fl, flFgi, csts)


instance Arbitrary Update where
  arbitrary = sized $ \n -> do

    undefined


spec :: Spec
spec = describe "Statistics " $ do
  it "prop_TODO" $ property True

propBlockFt :: Update -> Order -> Double
propBlockFt = undefined


--
-- StatisticsSpec.hs ends here
