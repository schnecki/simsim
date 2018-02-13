-- Block.hs ---
--
-- Filename: Block.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:03:43 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 31
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

module SimSim.Runner.Block
    ( machine
    , queue
    ) where

import           ClassyPrelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
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
import           SimSim.Runner.Util
import           SimSim.Simulation.Type


queue :: (MonadIO m) =>
         Routing -> Order -> Proxy Block Order Block Order (StateT SimSim m) ()
queue routes order = do
  case nextMachine order of
    q@Queue{} -> do
      block <- respond $ dispatch routes $ order { lastMachine = q }
      return ()
    _ -> void $ respond order

  nxtOrder <- request (lastMachine order)
  queue routes nxtOrder


-- | Machines push the finished orders to the dispatcher and pull order from the
-- queue.
machine :: (MonadIO m) =>
           Routing -> Order -> Proxy Block Order Block Order (StateT SimSim m) ()
machine routes order = do

  endTime <- getSimEndTime

  case nextMachine order of
    m@Machine{} -> do           -- for us, thus produce and then push downstream
      -- empty machine
      pT <- getProcessingTime m order


      let order' = dispatch routes $ order { lastMachine = m }
      block <- respond order'


      -- request new order, and produce
      return ()
    _        -> do              -- not for us, just push downstream
      block <- respond order
      return ()

  nxtOrder <- request (lastMachine order)
  machine routes nxtOrder


--
-- Block.hs ends here
