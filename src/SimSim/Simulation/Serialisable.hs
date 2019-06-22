{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE RankNTypes          #-}
-- Serialisable.hs ---
--
-- Filename: Serialisable.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Jun 19 15:40:51 2019 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 16
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


module SimSim.Simulation.Serialisable where

import           ClassyPrelude
import           Data.Graph
import qualified Data.List.NonEmpty         as NL
import qualified Data.Map.Strict            as M
import           Data.Serialize
import           Data.Vector.Serialize      ()
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics
import qualified Prelude                    as Prelude
import           System.Random
import           System.Random.MWC


import           SimSim.Block
import           SimSim.BlockTimes
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

data SimSimSerialisable = SimSimSerialisable
  { serSimRouting         :: !Routing
  , serSimCurrentTime     :: !Time
  , serSimPeriodLength    :: !PeriodLength
  , serSimNextOrderId     :: !OrderId
  -- , simRelease         :: !Release
  -- , simDispatch        :: !Dispatch
  -- , simShipment        :: !Shipment
  , serSimOrdersOrderPool :: ![Order]
  , serSimOrdersQueue     :: !(M.Map Block [Order])
  , serSimOrdersMachine   :: !(M.Map Block (Order, Time)) -- ^ Order and left over processing time for this order.
  , serSimOrdersFgi       :: ![Order]
  , serSimOrdersShipped   :: ![Order] -- ^ Orders which have been shipped in last period.
  , serSimStatistics      :: SimStatistics
  , serSimInternal        :: !SimInternalSerialisable
  } deriving (Eq, Ord, Generic, Serialize)


data SimInternalSerialisable = SimInternalSerialisable
  { serSimBlocks              :: !(NL.NonEmpty Block)
  , serSimBlockTimes          :: !BlockTimes
  , serSimEndTime             :: !Time
  , serSimMaxMachines         :: !Int
  -- , simProcessingTimes :: !ProcessingTimes
  -- , serSimRandomNumbers  :: !(NL.NonEmpty Double)
  , serSimRandGen             :: !(V.Vector Word32)
  , serSimOrderGenerationTime :: Time
  , serSimProductRoutes       :: !(M.Map ProductType [Block])
  , serSimBlockLastOccur      :: !(M.Map Block Int)
  } deriving (Eq, Ord, Generic, Serialize)


--
-- Serialisable.hs ends here
