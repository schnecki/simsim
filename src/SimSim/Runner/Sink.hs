{-# LANGUAGE TemplateHaskell #-}
-- Sink.hs ---
--
-- Filename: Sink.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jul 28 10:48:47 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 41
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

module SimSim.Runner.Sink
    ( sink
    ) where

import           ClassyPrelude              hiding (replicate)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Class
import           Data.Sequence              (replicate)
import           Data.Text                  (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude              as Pipe
import           System.Random


import           SimSim.Block
import           SimSim.Order
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Util
import           SimSim.Simulation
import           SimSim.Simulation.Type
import           SimSim.Statistics
import           SimSim.Time


sink :: (MonadLogger m, MonadIO m) => Downstream -> Client Block Downstream (StateT SimSim m) ()
sink (Left nr) = logger Nothing "No more orders: END OF SIMULATION"
sink (Right order) = do
  case nextBlock order of
    Sink -> return ()           -- everything worked out
    bl   -> error $  "non Sink order at sink!!!: " ++ show order
  request Sink >>= sink


--
-- Sink.hs ends here
