-- Machine.hs ---
--
-- Filename: Machine.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:03:43 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 119
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

module SimSim.Runner.Machine
    ( machine
    ) where

import           ClassyPrelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Class
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude              as Pipe
import           System.Random


import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.ProcessingTime
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Dispatch
import           SimSim.Runner.Util
import           SimSim.Simulation.Type


-- | Machines push the finished orders to the dispatcher and pull order from the queue.
machine :: (MonadIO m) => Routing -> Downstream -> Proxy Block Downstream Block Downstream (StateT SimSim m) ()
machine _ (Left nr) = do
  putStrLn "\n\nempty machine pipe\n\n"
  void $ respond $ Left (nr+1)
machine routes (Right order) = do
  let m = nextBlock order
  case m of
    Machine {} -> do
      endTime <- getSimEndTime
      blockTime <- getBlockTime m
      if blockTime >= endTime
        then respond (pure order)                        -- nothing to do no more
        else process routes m order >>= respond . pure   -- for us, process
    _ -> respond (pure order)                            -- not for us, push through
  nxtOrder <- request m                                  -- send upstream that we are free (request new order)
  machine routes nxtOrder                                -- process next order


process :: (MonadState SimSim m) => Routing -> Block -> Order -> m Order
process routes m order = do
  pT <- getProcessingTime m order
  tStart <- getBlockTime m
  addToBlockTime m pT
  let order' = dispatch routes m $ setOrderCurrentTime (tStart+pT) $ setProdStartTime tStart order
  return order'


--
-- Machine.hs ends here
