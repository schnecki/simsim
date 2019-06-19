{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
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
--     Update #: 83
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
import           Data.Serialize
import           GHC.Generics


import           SimSim.Block

data SimStatistics = SimStatistics
  { simStatsBlockFlowTimes  :: M.Map Block StatsFlowTime -- ^ Statistics for blocks, like nr of orders, flow time.
  , simStatsBlockProcTimes  :: M.Map Block StatsProcTime    -- ^ Lists the (processing) times only for the machines and queues.
  , simStatsShopFloor       :: StatsFlowTime       -- ^ Shop floor (from release until entry of finished goods inventory)
  , simStatsShopFloorAndFgi :: StatsFlowTime       -- ^ Shop floor (from release until shipping)
  , simStatsOrderCosts      :: StatsOrderCost -- ^ Nr of orders (costs) to pay split into earnings, wip, backorder and
                                              -- holding.
  } deriving (Show, Generic, Serialize)

instance Eq SimStatistics where
  stats1 == stats2 =
    and
      [ simStatsBlockFlowTimes stats1 == simStatsBlockFlowTimes stats2
      , simStatsBlockProcTimes stats1 == simStatsBlockProcTimes stats2
      , simStatsShopFloor stats1 == simStatsShopFloor stats2
      , simStatsShopFloorAndFgi stats1 == simStatsShopFloorAndFgi stats2
      , simStatsOrderCosts stats1 == simStatsOrderCosts stats2
      ]

data StatsFlowTime = StatsFlowTime
  { statsNrOrders       :: Integer              -- ^ Nr of orders.
  , statsOrderFlowTime  :: StatsOrderTime       -- ^ Flow time statistics.
  , statsOrderTardiness :: Maybe StatsOrderTard -- ^ Only tardy orders for shop floor and shop floor plus FGI.
  } deriving (Eq, Show, Generic, Serialize)

data StatsOrderTime = StatsOrderTime
  { statsSumTime           :: Rational
  , statsStdDevTime        :: Rational
  , statsLastUpdatePartial :: Maybe StatsOrderTime -- ^ Only used if last update was partial. Holds the previous
                                                   -- ``StatsOrderTime``.
  } deriving (Show, Generic, Serialize)

instance Eq StatsOrderTime where
  (StatsOrderTime sum1 stdDev1 _) == (StatsOrderTime sum2 stdDev2 _) = sum1 == sum2 && stdDev1 == stdDev2


data StatsOrderTard = StatsOrderTard
  { statsNrTardOrders   :: Integer
  , statsSumTardiness   :: Rational
  , statsStdDevTardTime :: Rational
  } deriving (Eq, Show, Generic, Serialize)

data StatsOrderCost = StatsOrderCost
  { statsEarnings :: Integer    -- ^ Nr of earnings (sum of finished orders).
  , statsWipCosts :: Integer    -- ^ Nr of WIP costs (sum of orders in WIP at end of period).
  , statsBoCosts  :: Integer    -- ^ Nr of back order costs (sum of orders overdue at end of period per period).
  , statsFgiCosts :: Integer    -- ^ Nr of holding costs (sum of orders in inventory at end of period).
  } deriving (Eq, Show, Generic, Serialize)

data StatsProcTime = StatsProcTime
  { statsBlockTime :: Rational  -- ^ Processing time for Machine, idle time for OrderPool, Queue and FGI.
  -- , statsBroken
  } deriving (Eq, Show, Generic, Serialize)


emptyStatistics :: SimStatistics
emptyStatistics = SimStatistics mempty mempty emptyStats emptyStats emptyStatsOrderCost

emptyStats :: StatsFlowTime
emptyStats = StatsFlowTime 0 emptyStatsOrderTime Nothing

emptyStatsOrderTime :: StatsOrderTime
emptyStatsOrderTime = StatsOrderTime 0 0 Nothing

emptyStatsOrderTard :: StatsOrderTard
emptyStatsOrderTard = StatsOrderTard 0 0 0

emptyStatsOrderCost :: StatsOrderCost
emptyStatsOrderCost = StatsOrderCost 0 0 0 0

emptyStatsProcTime :: StatsProcTime
emptyStatsProcTime = StatsProcTime 0

--
-- Type.hs ends here
