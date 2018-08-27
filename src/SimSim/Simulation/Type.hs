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
--     Update #: 395
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
  , simOrdersOrderPool :: ![Order]
  , simOrdersQueue     :: !(M.Map Block [Order])
  , simOrdersMachine   :: !(M.Map Block (Order, Time)) -- ^ Order and left over processing time for this order.
  , simOrdersFgi       :: ![Order]
  , simOrdersShipped   :: ![Order] -- ^ Orders which have been shipped in last period.
  , simStatistics      :: SimStatistics
  , simInternal        :: !SimInternal
  }

instance Show SimSim where
  show sim =
    show (simRouting sim) ++ ", " ++ show (simCurrentTime sim) ++ ", " ++ show (simPeriodLength sim) ++ ", " ++ show (simNextOrderId sim) ++ ", " ++ show (simOrdersOrderPool sim) ++ ", " ++
    show (simOrdersFgi sim) ++ ", " ++ show (simOrdersMachine sim) ++ ", " ++ show (simOrdersQueue sim) ++ ", " ++ show (simOrdersShipped sim) ++ ", " ++ show (simStatistics sim)

instance Eq SimSim where
  sim1 == sim2 =
    and
      [ simRouting sim1 == simRouting sim2
      , simCurrentTime sim1 == simCurrentTime sim2
      , simPeriodLength sim1 == simPeriodLength sim2
      , simNextOrderId sim1 == simNextOrderId sim2
      , simOrdersOrderPool sim1 == simOrdersOrderPool sim2
      , simOrdersFgi sim1 == simOrdersFgi sim2
      , simOrdersMachine sim1 == simOrdersMachine sim2
      , M.filter (not.null) (simOrdersQueue sim1) == M.filter (not.null) (simOrdersQueue sim2)
      , simOrdersShipped sim1 == simOrdersShipped sim2
      , simStatistics sim1 == simStatistics sim2
      ]


type BlockTimes = M.Map Block Time

data SimInternal = SimInternal
  { simBlocks          :: !(NL.NonEmpty Block)
  , simBlockTimes      :: !BlockTimes
  , simEndTime         :: !Time
  , simMaxMachines     :: !Int
  , simProcessingTimes :: !ProcessingTimes
  , simRandomNumbers   :: !(NL.NonEmpty Double)
  , simProductRoutes   :: !(M.Map ProductType [Block])
  , simBlockLastOccur  :: !(M.Map Block Int)
  }


--
-- Type.hs ends here
