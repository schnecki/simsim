-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Jul 31 13:58:51 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 136
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

module SimSim.Statistics.Ops
    ( statsAddRelease
    , statsAddEndProduction
    , statsAddShipped
    , Update (..)
    ) where

import           ClassyPrelude
import qualified Data.Map.Strict            as M

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Simulation.Type
import           SimSim.Statistics.Internal
import           SimSim.Statistics.Type
import           SimSim.Time

statsAddRelease :: Order -> SimSim -> SimSim
statsAddRelease order sim = sim {simStatistics = updateBlockOrder (UpBlock OrderPool) order (simStatistics sim)}

statsAddEndProduction :: Order -> SimSim -> SimSim
statsAddEndProduction order sim = sim {simStatistics = updateShopFloorOrder EndProd order (simStatistics sim)}

statsAddShipped :: Order -> SimSim -> SimSim
statsAddShipped order sim = sim {simStatistics = updateBlockOrder (UpBlock FGI) order $ updateShopFloorAndFgiOrder Shipped order (simStatistics sim)}


updateShopFloorAndFgiOrder :: Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorAndFgiOrder up order simStatistics = simStatistics { simStatsShopFloorAndFgi = updateSimStatsOrder up order (simStatsShopFloorAndFgi simStatistics)
                                                                  , simStatsOrderCosts = updateCosts up order (simStatsOrderCosts simStatistics)
                                                                  }


updateShopFloorOrder :: Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorOrder up order simStatistics = simStatistics {simStatsShopFloor = updateSimStatsOrder up order (simStatsShopFloor simStatistics)}


updateBlockOrder :: Update -> Order -> SimStatistics -> SimStatistics
updateBlockOrder up@(UpBlock bl) order simStatistics = simStatistics {simStatsBlock = M.insert bl (updateSimStatsOrder up order stats) (simStatsBlock simStatistics)}
  where
    stats = fromMaybe emptyStats (M.lookup bl $ simStatsBlock simStatistics)


updateSimStatsOrder :: Update -> Order  -> SimStats -> SimStats
updateSimStatsOrder up order (SimStats nr ft tard) = SimStats (nr + 1) (updateOrderTime up order ft) (updateTardiness up order tard)


updateOrderTime :: Update -> Order -> StatsOrderTime -> StatsOrderTime
updateOrderTime up order (StatsOrderTime tSum stdDev) = StatsOrderTime (tSum + getBlockFlowTime up order) (stdDev)


--
-- Ops.hs ends here
