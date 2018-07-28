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
--     Update #: 182
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
import qualified Data.List.NonEmpty         as NL
import qualified Data.Map.Strict            as M
import           System.Random

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Period
import           SimSim.ProcessingTime.Type
import           SimSim.ProductType
import           SimSim.Release
import           SimSim.Routing
import           SimSim.Time

data SimSim =
  SimSim { simRouting        :: !Routing
         , simBlocks         :: !(NL.NonEmpty Block)
         , simCurrentTime    :: !Time
         , simPeriodLength   :: !PeriodLength
         , simNextOrderId    :: !OrderId
         , simRelease        :: !(Time -> [Order] -> IO [Order])
         , simFinishedOrders :: ![Order] -- ^ Orders which have been finished in last period.
         , simInternal       :: !SimInternal
         }


data SimInternal =
  SimInternal { simBlockTimes      :: !(M.Map Block Time)
              , simEndTime         :: !Time
              , maxMachines        :: !Int
              , simProcessingTimes :: !ProcessingTimes
              , randomNumbers      :: !(NL.NonEmpty Double)
              , simOrderPoolOrders :: ![Order]
              }


newSimSim :: (RandomGen g) => g -> Routes -> ProcTimes -> PeriodLength -> Release -> SimSim
newSimSim g routesE procTimes periodLen release =
  case NL.nonEmpty routesE of
    Nothing -> error "Routing cannot be empty, and must include a Source!"
    Just routes ->
      if hasSource
        then SimSim routes uniqueBlocks 0 periodLen 1 release mempty (SimInternal mTimes 1 maxMachines (fromProcTimes procTimes) randomNs mempty)
        else error "Routing must include a Source!"
      where uniqueBlocks = NL.fromList $ ordNub $ NL.toList allBlocks
            allBlocks = fmap snd routes <> fmap (snd . fst) routes
            maxMachines = maximum (impureNonNull $ 1 : lengths)
            lengths = fmap length $ NL.group $ NL.sortBy (compare `on` id) $ fmap (fst . fst) routes
            mTimes = M.fromList $ zip (toList allBlocks) (repeat 0)
            uniqueBlocksWSink
              | hasSink = uniqueBlocks
              | otherwise = NL.cons Sink uniqueBlocks
            hasSink = Sink `elem` uniqueBlocks
            hasSource = OrderPool `elem` uniqueBlocksWSink
            randomNs = NL.fromList $ randomRs (0, 1) g


setSimEndTime :: Time -> SimSim -> SimSim
setSimEndTime t sim = sim { simInternal = (simInternal sim) { simEndTime = t }}


setSimBlockTime :: Block -> Time -> SimSim -> SimSim
setSimBlockTime block t sim = sim {simInternal = (simInternal sim) {simBlockTimes = newM}}
  where
    newM = M.insert block t (simBlockTimes (simInternal sim))

updateTailRandNrs :: SimSim -> SimSim
updateTailRandNrs sim = sim {simInternal = (simInternal sim) {randomNumbers = newRands}}
  where
    newRands = NL.fromList $ NL.tail $ randomNumbers $ simInternal sim

setNextOrderId :: OrderId -> SimSim -> SimSim
setNextOrderId nextOrderId sim = sim { simNextOrderId = nextOrderId }

addNextOrderId :: OrderId -> SimSim -> SimSim
addNextOrderId add sim = sim { simNextOrderId = simNextOrderId sim + add }

addOrderToOrderPool :: Order -> SimSim -> SimSim
addOrderToOrderPool o sim = sim {simInternal = internal {simOrderPoolOrders = simOrderPoolOrders internal ++ [o]}}
  where
    internal = simInternal sim

removeOrdersFromOrderPool :: [Order] -> SimSim -> SimSim
removeOrdersFromOrderPool os sim = sim {simInternal = internal {simOrderPoolOrders = filter (`notElem` os) (simOrderPoolOrders internal)}}
  where
    internal = simInternal sim


--
-- Type.hs ends here
