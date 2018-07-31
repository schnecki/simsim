{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 20:28:23 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 336
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

module SimSim.Simulation.Type where

import           ClassyPrelude
import           Data.Graph
import qualified Data.List.NonEmpty         as NL
import qualified Data.Map.Strict            as M
import qualified Prelude                    as Prelude
import           System.Random

import           SimSim.Block
import           SimSim.Dispatch
import           SimSim.Order.Type
import           SimSim.Period
import           SimSim.ProcessingTime.Type
import           SimSim.ProductType
import           SimSim.Release
import           SimSim.Routing
import           SimSim.Shipment
import           SimSim.Statistics.Type
import           SimSim.Time

data SimSim = SimSim
  { simRouting         :: !Routing
  , simCurrentTime     :: !Time
  , simPeriodLength    :: !PeriodLength
  , simNextOrderId     :: !OrderId
  , simRelease         :: !Release
  , simDispatch        :: !Dispatch
  , simShipment        :: !Shipment
  , simOrderPoolOrders :: ![Order]
  , simOrdersQueue     :: !(M.Map Block [Order])
  , simOrdersMachine   :: !(M.Map Block (Order, Time)) -- ^ Order and left over processing time for this order.
  , simOrdersFgi       :: ![Order]
  , simOrdersFinished  :: ![Order] -- ^ Orders which have been finished in last period.
  , simStatistics      :: SimStatistics
  , simInternal        :: !SimInternal
  }


data SimInternal = SimInternal
  { simBlocks          :: !(NL.NonEmpty Block)
  , simBlockTimes      :: !(M.Map Block Time)
  , simEndTime         :: !Time
  , simMaxMachines     :: !Int
  , simProcessingTimes :: !ProcessingTimes
  , simRandomNumbers   :: !(NL.NonEmpty Double)
  , simProductRoutes   :: !(M.Map ProductType [Block])
  , simBlockLastOccur  :: !(M.Map Block Int)
  }


setSimEndTime :: Time -> SimSim -> SimSim
setSimEndTime t sim = sim { simInternal = (simInternal sim) { simEndTime = t }}

resetStatistics :: SimSim -> SimSim
resetStatistics sim = sim {simStatistics = emptyStatistics }

setSimCurrentTime :: Time -> SimSim -> SimSim
setSimCurrentTime t sim = sim { simCurrentTime = t }


setSimBlockTime :: Block -> Time -> SimSim -> SimSim
setSimBlockTime block t sim = sim {simInternal = (simInternal sim) {simBlockTimes = newM}}
  where
    newM = M.insert block t (simBlockTimes (simInternal sim))

updateTailRandNrs :: SimSim -> SimSim
updateTailRandNrs sim = sim {simInternal = (simInternal sim) {simRandomNumbers = newRands}}
  where
    newRands = NL.fromList $ NL.tail $ simRandomNumbers $ simInternal sim

setNextOrderId :: OrderId -> SimSim -> SimSim
setNextOrderId nextOrderId sim = sim { simNextOrderId = nextOrderId }

addNextOrderId :: OrderId -> SimSim -> SimSim
addNextOrderId add sim = sim { simNextOrderId = simNextOrderId sim + add }

addOrderToOrderPool :: Order -> SimSim -> SimSim
addOrderToOrderPool o sim = sim {simOrderPoolOrders = simOrderPoolOrders sim ++ [o]}

removeOrdersFromOrderPool :: [Order] -> SimSim -> SimSim
removeOrdersFromOrderPool os sim = sim {simOrderPoolOrders = filter (`notElem` os) (simOrderPoolOrders sim)}

addOrderToQueue :: Block -> Order -> SimSim -> SimSim
addOrderToQueue bl o sim = sim {simOrdersQueue = M.insertWith (flip (<>)) (nextBlock o) [o] (simOrdersQueue sim)}
  where internal = simInternal sim


getAndRemoveOrderFromQueue :: Block -> SimSim -> (Maybe Order, SimSim)
getAndRemoveOrderFromQueue bl sim = maybe def f (M.lookup bl (simOrdersQueue sim))
  where
    def = (Nothing, sim)
    blTime = fromMaybe (error $ "no blocktime for " ++ show bl) $ M.lookup bl (simBlockTimes $ simInternal sim)
    dispatchSort = simDispatch sim
    arrivedUntilDecision = (<= blTime) . orderCurrentTime
    f [] = def
    f xxs =
      case (dispatchSort bl (filter arrivedUntilDecision xxs), dispatchSort bl (filter (not . arrivedUntilDecision) xxs)) of
        ([], r:rs) -> (Just r, setSimBlockTime bl (orderCurrentTime r) $ sim {simOrdersQueue = M.insert bl rs (simOrdersQueue sim)})
        (x:xs, rs) -> (Just x, sim {simOrdersQueue = M.insert bl (xs ++ rs) (simOrdersQueue sim)})


addOrderToMachine :: Order -> Time -> SimSim -> SimSim
addOrderToMachine o time sim = sim { simOrdersMachine = M.insert (lastBlock o) (o,time) (simOrdersMachine sim) }

emptyOrdersMachine :: Block -> SimSim -> SimSim
emptyOrdersMachine bl sim = sim { simOrdersMachine = M.delete bl (simOrdersMachine sim)}


addOrderToFgi :: Order -> SimSim -> SimSim
addOrderToFgi o sim = sim {simOrdersFgi = simOrdersFgi sim <> [o] }

removeOrdersFromFgi :: [Order] -> SimSim -> SimSim
removeOrdersFromFgi os sim = sim {simOrdersFgi = filter (`notElem` os) (simOrdersFgi sim)}


getAndRemoveOrderFromMachine :: Block -> SimSim -> (Maybe (Order, Time), SimSim)
getAndRemoveOrderFromMachine bl sim = maybe def f (M.lookup bl (simOrdersMachine sim))
  where
    internal = simInternal sim
    def = (Nothing, sim)
    f res = (Just res, sim {simOrdersMachine = M.delete bl (simOrdersMachine sim)})


setFinishedOrders :: [Order] -> SimSim -> SimSim
setFinishedOrders xs sim = sim { simOrdersFinished = xs }

--
-- Type.hs ends here
