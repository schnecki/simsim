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
--     Update #: 210
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
    , statsAddBlock
    , statsAddBlockPartialUpdate
    , statsAddBlockBlockOnly
    , statsAddBlockTimesOnly
    ) where

import           ClassyPrelude
import qualified Data.Map.Strict            as M
import           Data.Ratio                 (denominator)

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Simulation.Type
import           SimSim.Statistics.Internal
import           SimSim.Statistics.Type
import           SimSim.Time


-- | This function reports the given order as released. The invariant is that the release date is set, otherwise an
-- error will be called.
statsAddRelease :: Order -> SimSim -> SimSim
statsAddRelease order sim = sim {simStatistics = updateBlockOrder False blTimes (UpBlock OrderPool) order (simStatistics sim)}
  where blTimes = simBlockTimes $ simInternal sim

-- | This function reports an order as finished with production (now entering the FGI). It updates the statistics
-- according to the given order.
statsAddEndProduction :: Order -> SimSim -> SimSim
statsAddEndProduction order sim
  | wasEmpty = sim {simStatistics = updateShopFloorOrder blTimes ProcTime EndProd order (simStatistics sim)}
  | otherwise = sim
  where
    blTimes = simBlockTimes $ simInternal sim
    wasEmpty = null $ simOrdersFgi sim

-- | This function reports an order as shipped (leaving the FGI). The corresponding statistical information is updated.
statsAddShipped :: Order -> SimSim -> SimSim
statsAddShipped order sim =
  sim {simStatistics = updateBlockOrder False blTimes (UpBlock FGI) order $ updateShopFloorAndFgiOrder blTimes Shipped order (simStatistics sim)}
  where blTimes = simBlockTimes $ simInternal sim

statsAddBlock :: Block -> Order -> SimSim -> SimSim
statsAddBlock bl o sim = statsAddBlockInternal False bl o sim


statsAddBlockPartialUpdate :: Block -> Order -> SimSim -> SimSim
statsAddBlockPartialUpdate bl o sim = statsAddBlockInternal True bl o sim

statsAddBlockInternal :: Bool -> Block -> Order -> SimSim -> SimSim
statsAddBlockInternal isPartial bl o sim
  | isMachine bl || isQueue bl = sim {simStatistics = updateBlockOrder isPartial blTimes (UpBlock bl) o (simStatistics sim)}
  | otherwise = error $ "statsAddBlock is only for machines and queues, but was used for block " ++ show bl ++ ". Use the other functions instead."
    where blTimes = simBlockTimes $ simInternal sim

statsAddBlockTimesOnly :: Block -> Order -> SimSim -> SimSim
statsAddBlockTimesOnly bl o sim
  | isMachine bl || isQueue bl = sim {simStatistics = updateBlockOrderTimesOnly blTimes (UpBlock bl) o (simStatistics sim)}
  | otherwise = error $ "statsAddBlockTimesOnly is only for machines and queues, but was used for block " ++ show bl ++ ". Use the other functions instead."
  where
    blTimes = simBlockTimes $ simInternal sim

statsAddBlockBlockOnly :: Bool -> Block -> Order -> SimSim -> SimSim
statsAddBlockBlockOnly isPartial bl o sim
  | isMachine bl || isQueue bl = sim {simStatistics = updateBlockOrderBlockOnly isPartial blTimes (UpBlock bl) o (simStatistics sim)}
  | otherwise = error $ "statsAddBlockBlockOnly is only for machines and queues, but was used for block " ++ show bl ++ ". Use the other functions instead."
  where
    blTimes = simBlockTimes $ simInternal sim


-- | This function accumulates the costs at the end of the period. If it is not the end of the period, then nothing is done.
statsEndPeriodAddCosts :: SimSim -> SimSim
statsEndPeriodAddCosts sim
  | denominator (fromTime (simCurrentTime sim) / fromTime (simPeriodLength sim)) == 0 =
    sim {simStatistics = updateCostsEndPeriod (simCurrentTime sim) (simOrdersOrderPool sim) (simOrdersQueue sim) (simOrdersMachine sim) (simOrdersFgi sim) (simStatistics sim)}
  | otherwise = sim


updateCostsEndPeriod :: Time -> [Order] -> M.Map Block [Order] -> M.Map Block (Order,Time) -> [Order] -> SimStatistics -> SimStatistics
updateCostsEndPeriod curTime opOrds queueOrds machineOrds fgiOrds simStatistics = simStatistics { simStatsOrderCosts = addCosts (simStatsOrderCosts simStatistics) }
  where addCosts (StatsOrderCost ear wip bo fgi) = StatsOrderCost ear (wip + fromIntegral (length wipOrds)) (bo + fromIntegral (length boOrds)) (fgi + fromIntegral (length fgiOrds))
        isBackOrder o = curTime >= dueDate o
        boOrds = filter isBackOrder (opOrds ++ wipOrds ++ fgiOrds)
        wipOrds = map fst (M.elems machineOrds) ++ concat (M.elems queueOrds)


updateShopFloorAndFgiOrder :: BlockTimes -> Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorAndFgiOrder blTimes up order simStatistics =
  simStatistics
  {simStatsShopFloorAndFgi = updateSimFlowTimeStatsOrder False blTimes up order (simStatsShopFloorAndFgi simStatistics), simStatsOrderCosts = updateCosts up order (simStatsOrderCosts simStatistics)}


updateShopFloorOrder :: BlockTimes -> UpdateType -> Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorOrder blTimes ProcTime up order simStatistics = simStatistics {simStatsShopFloor = updateSimFlowTimeStatsOrder False blTimes up order (simStatsShopFloor simStatistics)}


updateBlockOrder :: Bool -> BlockTimes -> Update -> Order -> SimStatistics -> SimStatistics
updateBlockOrder isPartial blTimes up@(UpBlock bl) order = updateBlockOrderBlockOnly isPartial blTimes up order . updateBlockOrderTimesOnly blTimes up order
updateBlockOrder _ _ bl _ = error ("called updateBlockOrder on a non block: " ++ show bl)


updateBlockOrderBlockOnly :: Bool -> BlockTimes -> Update -> Order -> SimStatistics -> SimStatistics
updateBlockOrderBlockOnly isPartial blTimes up@(UpBlock bl) order simStatistics =
  simStatistics {simStatsBlockFlowTimes = M.insert bl (updateSimFlowTimeStatsOrder isPartial blTimes up order stats) (simStatsBlockFlowTimes simStatistics)}
  where
    stats = fromMaybe emptyStats (M.lookup bl $ simStatsBlockFlowTimes simStatistics)
updateBlockOrderBlockOnly _ _ bl _ _ = error ("called updateBlockOrderBlockOnly on a non block: " ++ show bl)


updateBlockOrderTimesOnly :: BlockTimes -> Update -> Order -> SimStatistics -> SimStatistics
updateBlockOrderTimesOnly blTimes up@(UpBlock bl) order simStatistics =
  simStatistics {simStatsBlockProcTimes = M.insert bl (updateStatsProcTime blTimes up order statsBlTimes) (simStatsBlockProcTimes simStatistics)}
  where
    statsBlTimes = fromMaybe emptyStatsProcTime (M.lookup bl $ simStatsBlockProcTimes simStatistics)
updateBlockOrderTimesOnly _ bl _ _ = error ("called updateBlockOrderTimesOnly on a non block: " ++ show bl)


updateStatsProcTime :: BlockTimes -> Update -> Order -> StatsProcTime -> StatsProcTime
updateStatsProcTime blTimes up@(UpBlock (Machine {})) order (StatsProcTime pT) = StatsProcTime (pT + getBlockFlowTime blTimes up order)
updateStatsProcTime blTimes up@(UpBlock bl) order (StatsProcTime pT) = StatsProcTime (pT + fromTime (max 0 $ blockStartTime order - M.findWithDefault 0 bl blTimes))

-- | This function updates the ``SimFlowTimeStats`` according to the given order and for the given block. The Boolean, decides
-- whether the order is counted (False) or if this is a partial update (True) and the actual one will follow.
updateSimFlowTimeStatsOrder :: Bool -> BlockTimes -> Update -> Order  -> SimFlowTimeStats -> SimFlowTimeStats
updateSimFlowTimeStatsOrder isPartial blTimes up order (SimFlowTimeStats nr ft mTard) = case up of
  UpBlock{} -> SimFlowTimeStats nr' (updateOrderTime isPartial blTimes up order ft) Nothing
  _         -> SimFlowTimeStats nr' (updateOrderTime isPartial blTimes up order ft) (Just $ updateTardiness up order (fromMaybe emptyStatsOrderTard mTard))
  where nr' | isPartial = nr
            | otherwise = nr+1


updateOrderTime :: Bool -> BlockTimes -> Update -> Order -> StatsOrderTime -> StatsOrderTime
updateOrderTime isPartial blTimes up order st@(StatsOrderTime tSum stdDev _) =
  StatsOrderTime
    (tSum + getBlockFlowTime blTimes up order)
    (stdDev)
    (if isPartial
       then Just $ st {statsLastUpdatePartial = Nothing}
       else Nothing)


--
-- Ops.hs ends here
