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
--     Update #: 140
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
import           SimSim.Time


-- | Machines push the finished orders to the dispatcher and pull order from the queue.
machine :: (MonadIO m) => Routing -> Downstream -> Proxy Block Downstream Block Downstream (StateT SimSim m) ()
machine _ (Left nr) = do
  putStrLn "\n\nempty machine pipe\n\n"
  void $ respond $ Left (nr+1)
machine routes (Right order) = do
  let bl = nextBlock order
  case bl of
    Machine {} -> do
      processCurrentMachineWip routes bl
      pT <- getProcessingTime bl order
      processOrder routes bl order pT
    _ -> void $ respond (pure order) -- not for us, push through
  nxtOrder <- request bl -- send upstream that we are free (request new order)
  machine routes nxtOrder -- process next order

-- | Load order from machine and process.
processCurrentMachineWip :: (MonadIO m) => Routing -> Block -> Proxy Block Downstream Block Downstream (StateT SimSim m) ()
processCurrentMachineWip routes bl = do
  mOrderTime <- state (getAndRemoveOrderFromMachine bl)
  maybe (return ()) (uncurry $ processOrder routes bl) mOrderTime


processOrder :: (MonadIO m) => Routing -> Block -> Order -> Time -> Proxy Block Downstream Block Downstream (StateT SimSim m) ()
processOrder routes bl order pT = do
  endTime <- getSimEndTime
  blockTime <- getBlockTime bl
  if blockTime >= endTime
    then void $ respond (pure order)
    else if blockTime + pT > endTime -- check if can be processed fully
           then do
             addToBlockTime bl (endTime - blockTime)
             let order' = dispatch routes bl $ setOrderCurrentTime endTime $ setProdStartTime blockTime order
             modify (addOrderToMachine order' (pT + blockTime - endTime))
           else do
             addToBlockTime bl pT
             let order' = dispatch routes bl $ setOrderCurrentTime (blockTime + pT) $ setProdStartTime blockTime order
             void $ respond $ pure order' -- for us, process


--
-- Machine.hs ends here
