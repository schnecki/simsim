-- Util.hs ---
--
-- Filename: Util.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Nov 21 11:19:38 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 12
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

module SimSim.Runner.Util where

import           ClassyPrelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.List.NonEmpty               as NL
import qualified Data.Map.Strict                  as M
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude                    as Pipe
import           System.Random

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Dispatch
import           SimSim.Simulation.Type
import           SimSim.Time


getSimEndTime :: (Monad m) => Proxy Block Order Block Order (StateT SimSim m) Time
getSimEndTime = do
  sim <- lift get
  return $ simEndTime $ simInternal sim


getNextRand :: (Monad m) => Proxy Block Order Block Order (StateT SimSim m) Double
getNextRand = do
  sim <- lift get
  lift $ put $ updateTailRandNrs sim
  return $ NL.head $ randomNumbers $ simInternal sim

getProcessingTime :: (Monad m) => Block -> Order
                  -> Proxy Block Order Block Order (StateT SimSim m) (Maybe Time)
getProcessingTime block order = do
  r <- getNextRand
  sim <- lift get
  let f = do m <- M.lookup block (simProcessingTimes (simInternal sim))
             M.lookup (productType order) m
  return $ f <*> Just r


--
-- Util.hs ends here
