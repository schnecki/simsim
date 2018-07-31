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
--     Update #: 40
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
  , simStatsShopFloor       :: SimStats -- ^ Shop floor (from release until entry of finished goods inventory)
  , simStatsShopFloorAndFgi :: SimStats -- ^ Shop floor (from release until shipping)
  , simStatsOrderCosts      :: StatsOrderCost
  }

data SimStats = SimStats
  { statsNrOrders       :: Integer
  , statsOrderFlowTime  :: StatsOrderTime
  , statsOrderTardiness :: StatsOrderTard
  }

data StatsOrderTime = StatsOrderTime
  { statsSumTime    :: Double
  , statsStdDevTime :: Double
  }

data StatsOrderTard = StatsOrderTard
  { statsNrTardOrders   :: Integer
  , statsSumTardiness   :: Double
  , statsStdDevTardTime :: Double
  }

data StatsOrderCost = StatsOrderCost
  { statsEarnings :: Double
  , statsWipCosts :: Double
  , statsBoCosts  :: Double
  , statsFgiCosts :: Double
  }

data StatsBlockTime = StatsBlockTime
  { statsProcessing :: Double
  -- , statsBroken
  }


emptyStatistics :: SimStatistics
emptyStatistics = SimStatistics mempty mempty emptyStats emptyStats emptyStatsOrderCost

emptyStats :: SimStats
emptyStats = SimStats 0 emptyStatsOrderTime emptyStatsOrderTard

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
