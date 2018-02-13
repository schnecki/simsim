-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Nov 21 11:32:14 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 5
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

module SimSim.ProcessingTime.Ops where

import           ClassyPrelude
import           Control.Monad.Trans.State
import qualified Data.Map.Strict           as M
import           Pipes
import           System.Random

import           SimSim.Block
import           SimSim.Order
import           SimSim.ProductType
import           SimSim.Simulation
import           SimSim.Time


getProcTimeValue :: Block -> ProductType
                 -> Proxy Block Order Block Order (StateT (SimSim g) m) Time
getProcTimeValue block pt = do
  sim <- lift get
  let f = simProcessingTimes $ simInternal sim
  undefined


--
-- Ops.hs ends here
