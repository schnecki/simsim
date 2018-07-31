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
--     Update #: 6
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
  { simStatsOrderPool  :: SimStats
  , simStatsBlock      :: M.Map Block SimStats
  , simStatsProduction :: SimStats
  , simStatsSystem     :: SimStats
  }

data SimStats = SimStats
  { statsFlowTime  :: StatsTime
  , statsTardiness :: StatsTime
  , statsCosts     :: StatsCost
  }

data StatsTime = StatsTime
  { statsNrOrders      :: Integer
  , statsSumFlowTime   :: Double
  , statsStDevFlowTime :: Double
  }

data StatsCost = StatsCost
  { statsEarnings :: Double
  , statsWipCosts :: Double
  , statsBoCosts  :: Double
  , statsFgiCosts :: Double
  }


emptyStatistics :: SimStatistics
emptyStatistics = SimStatistics emptyStats mempty emptyStats emptyStats

emptyStats :: SimStats
emptyStats = SimStats emptyStatsTime emptyStatsTime emptyStatsCost

emptyStatsTime :: StatsTime
emptyStatsTime = StatsTime 0 0 0

emptyStatsCost :: StatsCost
emptyStatsCost = StatsCost 0 0 0 0


--
-- Type.hs ends here
