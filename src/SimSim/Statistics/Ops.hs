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
--     Update #: 164
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
    , statsEndPeriodAddCosts
    ) where

import           ClassyPrelude
import qualified Data.Map.Strict            as M

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Simulation.Type
import           SimSim.Statistics.Internal
import           SimSim.Statistics.Type
import           SimSim.Time


-- | This function reports the given order as released. The invariant is that the release date is set, otherwise an
-- error will be called.
statsAddRelease :: Order -> SimSim -> SimSim
statsAddRelease order sim = sim {simStatistics = updateBlockOrder (UpBlock OrderPool) order (simStatistics sim)}

-- | This function reports an order as finished with production (now entering the FGI). It updates the statistics
-- according to the given order.
statsAddEndProduction :: Order -> SimSim -> SimSim
statsAddEndProduction order sim = sim {simStatistics = updateShopFloorOrder EndProd order (simStatistics sim)}

-- | This function reports an order as shipped (leaving the FGI). The corresponding statistical information is updated.
statsAddShipped :: Order -> SimSim -> SimSim
statsAddShipped order sim =
  sim {simStatistics = updateBlockOrder (UpBlock FGI) order $ updateShopFloorAndFgiOrder Shipped order (simStatistics sim)}

-- | This function accumulates the costs at the end of the period.
statsEndPeriodAddCosts :: SimSim -> SimSim
statsEndPeriodAddCosts sim =
  sim {simStatistics = updateCostsEndPeriod (simCurrentTime sim) (simOrdersOrderPool sim) (simOrdersQueue sim) (simOrdersMachine sim) (simOrdersFgi sim) (simStatistics sim)}


updateCostsEndPeriod :: Time -> [Order] -> M.Map Block [Order] -> M.Map Block (Order,Time) -> [Order] -> SimStatistics -> SimStatistics
updateCostsEndPeriod curTime opOrds queueOrds machineOrds fgiOrds simStatistics = simStatistics { simStatsOrderCosts = addCosts (simStatsOrderCosts simStatistics) }
  where addCosts (StatsOrderCost ear wip bo fgi) = StatsOrderCost ear (wip + fromIntegral (length wipOrds)) (bo + fromIntegral (length boOrds)) (fgi + fromIntegral (length fgiOrds))
        isBackOrder o = curTime >= dueDate o
        boOrds = filter isBackOrder (opOrds ++ wipOrds ++ fgiOrds)
        wipOrds = map fst (M.elems machineOrds) ++ concat (M.elems queueOrds)


updateShopFloorAndFgiOrder :: Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorAndFgiOrder up order simStatistics =
  simStatistics
  {simStatsShopFloorAndFgi = updateSimStatsOrder up order (simStatsShopFloorAndFgi simStatistics), simStatsOrderCosts = updateCosts up order (simStatsOrderCosts simStatistics)}


updateShopFloorOrder :: Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorOrder up order simStatistics = simStatistics {simStatsShopFloor = updateSimStatsOrder up order (simStatsShopFloor simStatistics)}


updateBlockOrder :: Update -> Order -> SimStatistics -> SimStatistics
updateBlockOrder up@(UpBlock bl) order simStatistics =
  simStatistics
  { simStatsBlock = M.insert bl (updateSimStatsOrder up order stats) (simStatsBlock simStatistics)
  , simStatsBlockTimes = M.insert bl (updateStatsBlockTime up order blTimes) (simStatsBlockTimes simStatistics)
  }
  where
    stats = fromMaybe emptyStats (M.lookup bl $ simStatsBlock simStatistics)
    blTimes = fromMaybe emptyStatsBlockTime (M.lookup bl $ simStatsBlockTimes simStatistics)
updateBlockOrder bl _ _ = error ("called updateBlockOrder on a non block: " ++ show bl)

updateStatsBlockTime :: Update -> Order -> StatsBlockTime -> StatsBlockTime
updateStatsBlockTime up order (StatsBlockTime pT) = StatsBlockTime (pT + getBlockFlowTime up order)


updateSimStatsOrder :: Update -> Order  -> SimStats -> SimStats
updateSimStatsOrder up order (SimStats nr ft mTard) = case up of
  UpBlock{} -> SimStats (nr + 1) (updateOrderTime up order ft) Nothing
  _ ->  SimStats (nr + 1) (updateOrderTime up order ft) (Just $ updateTardiness up order (fromMaybe emptyStatsOrderTard mTard))


updateOrderTime :: Update -> Order -> StatsOrderTime -> StatsOrderTime
updateOrderTime up order (StatsOrderTime tSum stdDev) = StatsOrderTime (tSum + getBlockFlowTime up order) (stdDev)


--
-- Ops.hs ends here
