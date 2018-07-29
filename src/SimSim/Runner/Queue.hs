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
--     Update #: 97
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
queue routes (Left nr) = do     -- no more orders from upstream
  ptRoutes <- gets (simProductRoutes . simInternal)
  let getBlockNr xs
        | length xs <= nr = []
        | otherwise = [Prelude.head $ drop nr xs]
  let blocks = concatMap getBlockNr (M.elems ptRoutes)
  mQueues <- gets (simQueueOrders . simInternal)
  let orders = mconcat $ mapMaybe (`M.lookup` mQueues) blocks
  -- WHAT block?
  if null blocks || null orders
    then void $ respond $ Left (nr + 1)
    else sendAny routes nr blocks

    -- let order = Prelude.head orders
    -- modify (on)
    -- b <- respond (pure order)
    -- M.lookup mQueues
    -- void $ respond $ Left (nr+1)

queue routes (Right order) = do -- received an order, store and continue
  let q = nextBlock order
  case q of
    Queue {} -- process and add to queue list
     -> do
      let order' = processOrder routes q order
      modify $ addOrderToQueue q order'
    _ -> void $ respond (pure order) -- just push through

  request q >>= queue routes         -- request new order and process


processOrder :: Routing -> Block -> Order -> Order
processOrder routes q order = dispatch routes q order

sendAny routes nr [] = void $ respond $ Left (nr+1)
sendAny routes nr (bl:bs) = do
  mOrder <- state (getAndRemoveOrderFromQueue bl)
  maybe (sendAny bs) (\order -> do b <- respond $ pure order
                                   request b >>= queue routes

                         ) mOrder


--
-- Queue.hs ends here
