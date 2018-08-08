-- StatisticsSpec.hs ---
--
-- Filename: StatisticsSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  8 09:34:52 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Wed Aug  8 17:16:19 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 119
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
import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.Order
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Type
import           SimSim.Time

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
  arbitrary = sized $ \n -> oneof $ map return $ Shipped : EndProd : concatMap (map UpBlock . sizedBlocks) [1 .. n]
instance CoArbitrary Update where
  coarbitrary Shipped     = variant 0
  coarbitrary EndProd     = variant 1
  coarbitrary (UpBlock n) = variant (blockSizeNr n+1) . coarbitrary n


spec :: Spec
spec = describe "Statistics " $ do
  it "prop_blockFlowTime" $ property prop_blockFlowTime


reportResBlockFt :: Testable prop => Update -> Order -> (Double -> prop) -> Property
reportResBlockFt bl o prop =
  let res = getBlockFlowTime bl o
  in counterexample (show res) $ prop res

prop_blockFlowTime :: Update -> Order -> Property
prop_blockFlowTime bl@Shipped o = isJust (shipped o) ==> reportResBlockFt bl o $ \res -> fromTime (fromJust (shipped o) - fromJust (released o)) == res
prop_blockFlowTime bl@EndProd o = isJust (prodEnd o) ==> reportResBlockFt bl o $ \res -> fromTime (fromJust (prodEnd o) - fromJust (released o)) == res
prop_blockFlowTime bl@(UpBlock OrderPool) o = isJust (released o) ==> reportResBlockFt bl o $ \res -> fromTime (fromJust (released o) - arrivalDate o) == res
prop_blockFlowTime bl@(UpBlock FGI) o = isJust (shipped o) ==> reportResBlockFt bl o $ \res -> fromTime (fromJust (shipped o) - fromJust (prodEnd o)) == res
prop_blockFlowTime bl@(UpBlock Sink) o = expectFailure $ property $ getBlockFlowTime bl o > 0
prop_blockFlowTime bl o = property $ reportResBlockFt bl o $ \res -> fromTime (orderCurrentTime o - lastBlockStart o) == res


--
-- StatisticsSpec.hs ends here
