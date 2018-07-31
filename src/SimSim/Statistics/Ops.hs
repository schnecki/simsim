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
--     Update #: 112
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
    ) where

import           ClassyPrelude
import qualified Data.Map.Strict        as M

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Simulation.Type
import           SimSim.Statistics.Type
import           SimSim.Time

data Update = UpBlock Block | EndProd | Shipped


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
updateOrderTime up order (StatsOrderTime tSum stdDev) = StatsOrderTime (tSum + getBlockFt up order) (stdDev)


updateTardiness :: Update -> Order -> StatsOrderTard -> StatsOrderTard
updateTardiness up order st@(StatsOrderTard nr tardSum stdDev) =
  case up of
    EndProd ->
      case orderTardiness order of
        Nothing -> st
        Just x  -> StatsOrderTard (nr + 1) (tardSum + fromTime x) (stdDev)
    _ -> st


updateCosts :: Update -> Order -> StatsOrderCost -> StatsOrderCost
updateCosts up order st@(StatsOrderCost earn wip bo fgi) =
  case up of
    Shipped -> error "costs in statistics (updateCosts)"
    _       -> st


getBlockFt :: Update -> Order -> Double
getBlockFt bl order = case bl of
  UpBlock bl -> case bl of
    OrderPool -> fromTime $ fromMaybe err $ (-) <$> pure (arrivalDate order) <*> released order -- released
    FGI       -> fromTime $ fromMaybe err $ (-) <$> sent order <*> prodEnd order             -- released
    _         -> error "not yet implemented"                                                    -- machine or queue
  EndProd    -> fromTime $ fromMaybe err $ (-) <$> prodEnd order <*> released order             -- finished production
  Shipped    -> fromTime $ fromMaybe err $ (-) <$> sent order <*> released order                -- shipped

  where err = error "Nothing in getFt"


--
-- Ops.hs ends here
