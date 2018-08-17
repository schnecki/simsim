-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Jul 31 13:54:36 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 73
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

module SimSim.Statistics.Type where

import           ClassyPrelude
import qualified Data.Map.Strict as M

import           SimSim.Block

data SimStatistics = SimStatistics
  { simStatsBlock           :: M.Map Block SimStats       -- ^ Statistics for blocks, like nr of orders, flow time.
  , simStatsBlockTimes      :: M.Map Block StatsBlockTime -- ^ Lists the (processing) times only for the machines and queues.
  , simStatsShopFloor       :: SimStats       -- ^ Shop floor (from release until entry of finished goods inventory)
  , simStatsShopFloorAndFgi :: SimStats       -- ^ Shop floor (from release until shipping)
  , simStatsOrderCosts      :: StatsOrderCost -- ^ Nr of orders (costs) to pay split into earnings, wip, backorder and
                                              -- holding.
  } deriving (Show)

instance Eq SimStatistics where
  stats1 == stats2 =
    and
      [ simStatsBlock stats1 == simStatsBlock stats2
      , simStatsBlockTimes stats1 == simStatsBlockTimes stats2
      , simStatsShopFloor stats1 == simStatsShopFloor stats2
      , simStatsShopFloorAndFgi stats1 == simStatsShopFloorAndFgi stats2
      , simStatsOrderCosts stats1 == simStatsOrderCosts stats2
      ]

data SimStats = SimStats
  { statsNrOrders       :: Integer              -- ^ Nr of orders.
  , statsOrderFlowTime  :: StatsOrderTime       -- ^ Flow time statistics.
  , statsOrderTardiness :: Maybe StatsOrderTard -- ^ Only tardy orders for shop floor and shop floor plus FGI.
  } deriving (Eq, Show)

data StatsOrderTime = StatsOrderTime
  { statsSumTime           :: Rational
  , statsStdDevTime        :: Rational
  , statsLastUpdatePartial :: Maybe StatsOrderTime -- ^ Only used if last update was partial. Holds the previous
                                                   -- ``StatsOrderTime``.
  } deriving (Show)

instance Eq StatsOrderTime where
  (StatsOrderTime sum1 stdDev1 _) == (StatsOrderTime sum2 stdDev2 _) = sum1 == sum2 && stdDev1 == stdDev2


data StatsOrderTard = StatsOrderTard
  { statsNrTardOrders   :: Integer
  , statsSumTardiness   :: Rational
  , statsStdDevTardTime :: Rational
  } deriving (Eq, Show)

data StatsOrderCost = StatsOrderCost
  { statsEarnings :: Integer    -- ^ Nr of earnings (sum of finished orders).
  , statsWipCosts :: Integer    -- ^ Nr of WIP costs (sum of orders in WIP at end of period).
  , statsBoCosts  :: Integer    -- ^ Nr of back order costs (sum of orders overdue at end of period per period).
  , statsFgiCosts :: Integer    -- ^ Nr of holding costs (sum of orders in inventory at end of period).
  } deriving (Eq, Show)

data StatsBlockTime = StatsBlockTime
  { statsBlockTime :: Rational  -- ^ Processing time for machines, idle time for queues.
  -- , statsBroken
  } deriving (Eq, Show)


emptyStatistics :: SimStatistics
emptyStatistics = SimStatistics mempty mempty emptyStats emptyStats emptyStatsOrderCost

emptyStats :: SimStats
emptyStats = SimStats 0 emptyStatsOrderTime Nothing

emptyStatsOrderTime :: StatsOrderTime
emptyStatsOrderTime = StatsOrderTime 0 0 Nothing

emptyStatsOrderTard :: StatsOrderTard
emptyStatsOrderTard = StatsOrderTard 0 0 0

emptyStatsOrderCost :: StatsOrderCost
emptyStatsOrderCost = StatsOrderCost 0 0 0 0

emptyStatsBlockTime :: StatsBlockTime
emptyStatsBlockTime = StatsBlockTime 0

--
-- Type.hs ends here
