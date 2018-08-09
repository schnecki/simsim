-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:55:22 2018 (+0200)
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


module TestSimSim.Statistics.Instances
  (
  ) where


import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.Order
import           SimSim.Simulation.Type
import           SimSim.Statistics.Internal
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Type
import           SimSim.Time

import           TestSimSim.Block.Instances
import           TestSimSim.Order.Instances

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
  arbitrary = sized $ \n -> oneof $ map return $ Shipped : EndProd : concatMap (map UpBlock . sizedBlocks) [1 .. n]
instance CoArbitrary Update where
  coarbitrary Shipped     = variant 0
  coarbitrary EndProd     = variant 1
  coarbitrary (UpBlock n) = variant (blockSizeNr n+1) . coarbitrary n


--
-- Instances.hs ends here
