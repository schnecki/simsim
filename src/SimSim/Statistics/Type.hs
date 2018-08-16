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
--     Update #: 56
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
  { simStatsBlock           :: M.Map Block SimStats       -- ^ Statistics for blocks, like nr of orders.
  , simStatsBlockTimes      :: M.Map Block StatsBlockTime -- ^ Lists the (processing) times for the machines (only for machines).
  , simStatsShopFloor       :: SimStats       -- ^ Shop floor (from release until entry of finished goods inventory)
  , simStatsShopFloorAndFgi :: SimStats       -- ^ Shop floor (from release until shipping)
  , simStatsOrderCosts      :: StatsOrderCost -- ^ Nr of orders (costs) to pay split into earnings, wip, backorder and
                                              -- holding.
  } deriving (Eq, Show)

data SimStats = SimStats
  { statsNrOrders       :: Integer              -- ^ Nr of orders.
  , statsOrderFlowTime  :: StatsOrderTime       -- ^ Flow time statistics.
  , statsOrderTardiness :: Maybe StatsOrderTard -- ^ Only tardy orders for shop floor and shop floor plus FGI.
  } deriving (Eq, Show)

data StatsOrderTime = StatsOrderTime
  { statsSumTime    :: Rational
  , statsStdDevTime :: Rational
  } deriving (Eq, Show)

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
  { statsProcessing :: Rational
  -- , statsBroken
  } deriving (Eq, Show)


emptyStatistics :: SimStatistics
emptyStatistics = SimStatistics mempty mempty emptyStats emptyStats emptyStatsOrderCost

emptyStats :: SimStats
emptyStats = SimStats 0 emptyStatsOrderTime Nothing

emptyStatsOrderTime :: StatsOrderTime
emptyStatsOrderTime = StatsOrderTime 0 0

emptyStatsOrderTard :: StatsOrderTard
emptyStatsOrderTard = StatsOrderTard 0 0 0

emptyStatsOrderCost :: StatsOrderCost
emptyStatsOrderCost = StatsOrderCost 0 0 0 0

emptyStatsBlockTime :: StatsBlockTime
emptyStatsBlockTime = StatsBlockTime 0

--
-- Type.hs ends here
