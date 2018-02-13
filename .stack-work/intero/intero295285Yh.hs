{-# LANGUAGE AllowAmbiguousTypes #-}
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
--     Update #: 155
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
import           SimSim.Routing
import           SimSim.Time

data SimSim =
  SimSim { simRouting      :: Routing
         , simBlocks       :: NL.NonEmpty Block
         , simCurrentTime  :: Time
         , simPeriodLength :: PeriodLength
         , simNextOrderId  :: OrderId
         , simInternal     :: SimInternal
         }


data SimInternal =
  SimInternal { simBlockTimes      :: M.Map Block Time
              , simEndTime         :: Time
              , maxMachines        :: Int
              , simProcessingTimes :: ProcessingTimes
              , randomNumbers      :: NL.NonEmpty Double
              }


newSimSim :: (RandomGen g) => g -> Routes -> ProcTimes -> PeriodLength -> SimSim
newSimSim g routesE procTimes periodLen = case NL.nonEmpty routesE of
  Nothing -> error "Routing cannot be empty, and must include a Source!"
  Just routes -> if hasSource
    then SimSim routes uniqueBlocks 0 periodLen 1
         (SimInternal mTimes 0 maxMachines (fromProcTimes procTimes) randomNs)
    else error "Routing must include a Source!"

    where uniqueBlocks = NL.fromList $ ordNub $ NL.toList allBlocks
          allBlocks = fmap snd routes <> fmap (snd.fst) routes
          maxMachines = maximum (impureNonNull $ 1:lengths)
          lengths = fmap length $ NL.group $
                    NL.sortBy (compare `on` id) $ fmap (fst.fst) routes
          mTimes = M.fromList $ zip (toList allBlocks) [0..]
          uniqueBlocksWSink | hasSink = uniqueBlocks
                            | otherwise = NL.cons Sink uniqueBlocks
          hasSink = Sink `elem` uniqueBlocks
          hasSource = Source `elem` uniqueBlocksWSink
          randomNs = NL.fromList $ randomRs (0,1) g


updateSimEndTime :: Time -> SimSim -> SimSim
updateSimEndTime t sim = sim { simInternal = (simInternal sim) { simEndTime = t }}

updateSimBlockTime :: Block -> Time -> SimSim -> SimSim
updateSimBlockTime block t sim = sim { simInternal = (simInternal sim) { simBlockTimes = newM }}
  where newM = M.insert block t (simBlockTimes (simInternal sim))

updateTailRandNrs :: SimSim -> SimSim
updateTailRandNrs sim = sim { simInternal = (simInternal sim) { randomNumbers = newRands }}
  where newRands = NL.fromList $ NL.tail $ randomNumbers $ simInternal sim

updateNextOrderId :: OrderId -> SimSim -> SimSim
updateNextOrderId nextOrderId sim = sim { simNextOrderId = nextOrderId }

addNextOrderId :: OrderId -> SimSim -> SimSim
addNextOrderId add sim = sim { simNextOrderId = simNextOrderId sim + add }


--
-- Type.hs ends here
