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
--     Update #: 202
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

import           Control.Monad hiding (forM_)
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict hiding (foldM_,forM_)
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


queue :: (MonadIO m) => Text -> Routing -> Downstream -> Proxy Upstream Downstream Upstream Downstream (StateT SimSim m) ()
queue name routes (Left nr) -- no more orders from upstream, process queued orders
 = do
  liftIO $ putStrLn ("Left in " ++ tshow name ++ " with " ++ tshow nr)
  mQueues <- gets (simQueueOrders . simInternal)
  putStrLn $ "mQueues: " ++ tshow (fmap (map orderId) mQueues)
  -- load any order
  ptRoutes <- gets (simProductRoutes . simInternal)
  let getBlockNr xs
        | length xs <= (nr + 1) = []
        | otherwise = [Prelude.head $ drop (nr + 1) xs]
  let blocks = concatMap getBlockNr (M.elems ptRoutes)
  let orders = mconcat $ mapMaybe (`M.lookup` mQueues) blocks
  if null orders
    then void $ respond $ Left (nr + 1)
    else processBlocks True blocks >> void (respond $ Left (nr+1))
      
queue name routes (Right order)      -- received an order, store and continue
 = do
  liftIO $ putStrLn ("queue " ++ name ++ " input: " ++ tshow (orderId order))
  let q = nextBlock order
  case q of
    Queue {}                    -- process and add to queue list
     -> do
      let order' = processOrder routes q order
      modify $ addOrderToQueue q order'
      mQueues <- gets (simQueueOrders . simInternal)
      putStrLn $ "mQueues: " ++ tshow (fmap (map orderId) mQueues)
    
    _ -> void $ respond (pure order) -- just push through
  request q >>= queue name routes    -- request new order and process

-- | Process blocks, by responding one order for each block and the process the new requests.
processBlocks :: (MonadState SimSim m) => Bool -> [Block] -> Proxy x' x Block Downstream m [Block]
processBlocks _ [] = return []
processBlocks loop (bl:bs) = do
  mOrder <- state (getAndRemoveOrderFromQueueSim bl)
  case mOrder of
    Nothing -> processBlocks loop bs
    Just o -> do
      r <- respond $ pure o
      rs <- processBlocks False bs
      if loop
        then processBlocks True (r:rs)
        else return (r:rs)


processOrder :: Routing -> Block -> Order -> Order
processOrder routes q order = dispatch routes q order

-- processQueue :: (MonadIO m) => Text -> Routing -> Int -> Block -> Proxy Upstream Downstream Upstream Downstream (StateT SimSim m) ()
-- processQueue name routes nr bl = trace ("processQueue") $ do
--   mOrder <- state (getAndRemoveOrderFromQueue bl)
--   case mOrder of
--     Nothing    -> queue name routes (Left nr)
--     Just order -> respond (pure order) >>= processQueue name routes nr

-- getAnyOrder :: (MonadState SimSim m) => Int -> [Block] -> m (Either Int Order)
-- getAnyOrder nr []      = return $ Left (nr+1)
-- getAnyOrder nr (bl:bs) = do
--   mOrder <- state (getAndRemoveOrderFromQueue bl)
--   maybe (getAnyOrder nr bs) (return.pure) mOrder


--
-- Queue.hs ends here
