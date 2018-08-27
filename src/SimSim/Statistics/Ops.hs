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
--     Update #: 247
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
    , UpdateType (..)
    ) where

import           ClassyPrelude
import qualified Data.List                  as L
import qualified Data.Map.Strict            as M
import           Data.Ratio                 (denominator)

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Simulation.Type
import           SimSim.Statistics.Internal
import           SimSim.Statistics.Type
import           SimSim.Time


data UpdateType = FlowAndProcTime | FlowTime | ProcTime
  deriving (Show)


-- | This function reports the given order as released. The invariant is that the release date is set, otherwise an
-- error will be called.
statsAddRelease :: Order -> SimSim -> SimSim
statsAddRelease order sim = sim {simStatistics = updateBlockOrder False blTimes FlowAndProcTime (UpBlock OrderPool) order (simStatistics sim)} -- TODO: FlowAndProcTime is wrong
  where blTimes = simBlockTimes $ simInternal sim

-- | This function reports an order as finished with production (now entering the FGI). It updates the statistics
-- according to the given order.
statsAddEndProduction :: Order -> SimSim -> SimSim
statsAddEndProduction order sim = sim {simStatistics = updateShopFloorOrder blTimes EndProd order (simStatistics sim)}
  -- | wasEmpty = sim {simStatistics = updateBlockOrder False blTimes ProcTime (UpBlock FGI) order  }
  -- | otherwise = sim {simStatistics = updateShopFloorOrder blTimes EndProd order (simStatistics sim)}
  where
    blTimes = simBlockTimes $ simInternal sim
    wasEmpty = null $ simOrdersFgi sim

-- | This function reports a set of orders as shipped (leaving the FGI). And updates the processing time of the FGI
-- accordingly. The corresponding statistical information is updated.
statsAddShipped :: [Order] -> [Order] -> SimSim -> SimSim
statsAddShipped fgiOrds shippedOrders sim
  | wasEmpty = foldl' upOrder (sim {simStatistics = updateBlockOrder False blTimes ProcTime (UpBlock FGI) dummyOrder (simStatistics sim)}) shippedOrders
  | otherwise = foldl' upOrder sim shippedOrders
  where
    blTimes = simBlockTimes $ simInternal sim
    upOrder s order = s {simStatistics = updateBlockOrder False blTimes FlowTime (UpBlock FGI) order $ updateShopFloorAndFgiOrder blTimes Shipped order (simStatistics s)}
    earliestEntry = minimum $ simEndTime (simInternal sim) `ncons` map blockStartTime fgiOrds
    dummyOrder = (newOrder (Product 1) 0 0) {blockStartTime = earliestEntry}
    wasEmpty
      | earliestEntry > simCurrentTime sim = True
      | otherwise = False

statsAddBlock :: UpdateType -> Block -> Order -> SimSim -> SimSim
statsAddBlock = statsAddBlockInternal False

statsAddBlockPartialUpdate :: UpdateType -> Block -> Order -> SimSim -> SimSim
statsAddBlockPartialUpdate = statsAddBlockInternal True

statsAddBlockInternal :: Bool -> UpdateType -> Block -> Order -> SimSim -> SimSim
statsAddBlockInternal isPartial upType bl o sim = sim {simStatistics = updateBlockOrder isPartial blTimes upType (UpBlock bl) o (simStatistics sim)}
  where
    blTimes = simBlockTimes $ simInternal sim


-- | This function accumulates the costs at the end of the period. If it is not the end of the period, then nothing is done.
statsEndPeriodAddCosts :: SimSim -> SimSim
statsEndPeriodAddCosts sim
  | denominator (fromTime (simCurrentTime sim) / fromTime (simPeriodLength sim)) == 1 =
    sim {simStatistics = updateCostsEndPeriod (simCurrentTime sim) (simOrdersOrderPool sim) (simOrdersQueue sim) (simOrdersMachine sim) (simOrdersFgi sim) (simStatistics sim)}
  | otherwise = sim


-------------------- Helper Functions --------------------

updateCostsEndPeriod :: Time -> [Order] -> M.Map Block [Order] -> M.Map Block (Order,Time) -> [Order] -> SimStatistics -> SimStatistics
updateCostsEndPeriod curTime opOrds queueOrds machineOrds fgiOrds simStatistics = simStatistics { simStatsOrderCosts = addCosts (simStatsOrderCosts simStatistics) }
  where addCosts (StatsOrderCost ear wip bo fgi) = StatsOrderCost ear (wip + fromIntegral (length wipOrds)) (bo + fromIntegral (length boOrds)) (fgi + fromIntegral (length fgiOrds))
        isBackOrder o = curTime >= dueDate o
        boOrds = filter isBackOrder (opOrds ++ wipOrds ++ fgiOrds)
        wipOrds = map fst (M.elems machineOrds) ++ concat (M.elems queueOrds)


updateShopFloorAndFgiOrder :: BlockTimes -> Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorAndFgiOrder blTimes up order simStatistics =
  simStatistics
  {simStatsShopFloorAndFgi = updateStatsFlowTime False up order (simStatsShopFloorAndFgi simStatistics), simStatsOrderCosts = updateCosts up order (simStatsOrderCosts simStatistics)}


updateShopFloorOrder :: BlockTimes -> Update -> Order -> SimStatistics -> SimStatistics
updateShopFloorOrder blTimes up order simStatistics = simStatistics {simStatsShopFloor = updateStatsFlowTime False up order (simStatsShopFloor simStatistics)}


updateBlockOrder :: Bool -> BlockTimes -> UpdateType -> Update -> Order -> SimStatistics -> SimStatistics
updateBlockOrder isPartial blTimes upType up@(UpBlock bl) order simStatistics =
  case upType of
    ProcTime -> simStatistics {simStatsBlockProcTimes = M.insert bl (updateStatsProcTime blTimes up order statsBlTimes) (simStatsBlockProcTimes simStatistics)}
    FlowTime -> simStatistics {simStatsBlockFlowTimes = M.insert bl (updateStatsFlowTime isPartial up order stats) (simStatsBlockFlowTimes simStatistics)}
    FlowAndProcTime ->
      simStatistics
      { simStatsBlockProcTimes = M.insert bl (updateStatsProcTime blTimes up order statsBlTimes) (simStatsBlockProcTimes simStatistics)
      , simStatsBlockFlowTimes = M.insert bl (updateStatsFlowTime isPartial up order stats) (simStatsBlockFlowTimes simStatistics)
      }
  where
    stats = fromMaybe emptyStats (M.lookup bl $ simStatsBlockFlowTimes simStatistics)
    statsBlTimes = fromMaybe emptyStatsProcTime (M.lookup bl $ simStatsBlockProcTimes simStatistics)

updateBlockOrder _ _ _ bl _ _ = error ("called updateBlockOrder on a non block: " ++ show bl)


-- | This function updates the ``StatsProcTime
updateStatsProcTime :: BlockTimes -> Update -> Order -> StatsProcTime -> StatsProcTime
updateStatsProcTime _ up@(UpBlock Machine {}) order (StatsProcTime pT) = StatsProcTime (pT + getBlockFlowTime up order)
updateStatsProcTime blTimes up@(UpBlock bl) order (StatsProcTime pT) = StatsProcTime (pT + fromTime (max 0 $ blockStartTime order - blTime))
  where
    blTime = fromMaybe (error $ "empty block time for " ++ show bl ++ " in updateStatsProcTime") (M.lookup bl blTimes)


-- | This function updates the ``StatsFlowTime`` according to the given order and for the given block. The Boolean, decides
-- whether the order is counted (False) or if this is a partial update (True) and the actual one will follow.
updateStatsFlowTime :: Bool -> Update -> Order  -> StatsFlowTime -> StatsFlowTime
updateStatsFlowTime isPartial up order (StatsFlowTime nr ft mTard) = case up of
  UpBlock{} -> StatsFlowTime nr' (updateStatsOrderTime isPartial up order ft) Nothing
  _         -> StatsFlowTime nr' (updateStatsOrderTime isPartial up order ft) (Just $ updateTardiness up order (fromMaybe emptyStatsOrderTard mTard))
  where nr' | isPartial = nr
            | otherwise = nr+1


--
-- Ops.hs ends here
