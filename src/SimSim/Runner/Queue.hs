{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- Queue.hs ---
--
-- Filename: Queue.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:03:43 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 128
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

module SimSim.Runner.Queue
    ( queue
    ) where

import           ClassyPrelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict hiding (foldM_)
import           Control.Monad.Trans.Class
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude              as Pipe
import qualified Prelude                    as Prelude
import           System.Random

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Dispatch
import           SimSim.Runner.Util
import           SimSim.Simulation.Type

type QueueResponse = Maybe Order


queue :: (MonadIO m) => Routing -> Downstream -> Proxy Upstream Downstream Upstream Downstream (StateT SimSim m) ()
queue routes (Left nr) -- no more orders from upstream, process queued orders
 = do
  ptRoutes <- gets (simProductRoutes . simInternal)
  let getBlockNr xs
        | length xs <= nr = []
        | otherwise = [Prelude.head $ drop nr xs]
  let blocks = concatMap getBlockNr (M.elems ptRoutes)
  mQueues <- gets (simQueueOrders . simInternal)
  let orders = mconcat $ mapMaybe (`M.lookup` mQueues) blocks
  if null blocks || null orders
    then void $ respond $ Left (nr + 1)
    else do
      response <- getAnyOrder nr blocks
      bl <- respond response
      processQueue routes nr bl

queue routes (Right order) -- received an order, store and continue
 = do
  let q = nextBlock order
  case q of
    Queue {}                    -- process and add to queue list
     -> do
      let order' = processOrder routes q order
      modify $ addOrderToQueue q order'
    _ -> void $ respond (pure order) -- just push through
  request q >>= queue routes         -- request new order and process

processOrder :: Routing -> Block -> Order -> Order
processOrder routes q order = dispatch routes q order

processQueue :: (MonadIO m) => Routing -> Int -> Block -> Proxy Upstream Downstream Upstream Downstream (StateT SimSim m) ()
processQueue routes nr bl = do
  mOrder <- state (getAndRemoveOrderFromQueue bl)
  case mOrder of
    Nothing    -> queue routes (Left nr)
    Just order -> respond (pure order) >>= processQueue routes nr

getAnyOrder :: (MonadState SimSim m) => Int -> [Block] -> m (Either Int Order)
getAnyOrder nr []      = return $ Left (nr+1)
getAnyOrder nr (bl:bs) = do
  mOrder <- state (getAndRemoveOrderFromQueue bl)
  maybe (getAnyOrder nr bs) (return.pure) mOrder


--
-- Queue.hs ends here
