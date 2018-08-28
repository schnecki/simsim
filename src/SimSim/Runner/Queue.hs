{-# LANGUAGE TemplateHaskell #-}
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
--     Update #: 343
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

import           Control.Monad hiding (forM_, mapM_)
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict hiding (foldM_,forM_, mapM_)
import           Control.Monad.Trans.Class
import qualified Data.Map.Strict            as M
import           Data.Text                  (Text)
import           Data.Void
import qualified Data.List as L
import           Debug.Trace
import           Pipes
import Control.Monad.Logger
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude              as Pipe
import qualified Prelude                    as Prelude
import           System.Random

import           SimSim.Block
import SimSim.Statistics
import           SimSim.Order.Type
import           SimSim.ProcessingTime.Ops
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Dispatch
import           SimSim.Runner.Machine
import           SimSim.Runner.Util
import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type

type QueueResponse = Maybe Order


queue :: (MonadLogger m, MonadIO m) => Text -> Routing -> Downstream -> Proxy Upstream Downstream Upstream Downstream (StateT SimSim m) ()
queue name routes (Left nr) -- no more orders from upstream, process queued orders
 = do
  mQueues <- gets simOrdersQueue --  load any order
  logger Nothing $ "Queue " ++ name ++ " input Left " ++ tshow nr ++ ". current queues:" <> tshow (map orderId <$> mQueues)
  blLastOccur <- gets (simBlockLastOccur . simInternal)
  -- fill next block and possibly move orders into machine from last block
  let blocks = map fst $ filter ((\x -> x == nr + 1 || x == nr - 1) . snd) (M.toList blLastOccur)
  -- logger Nothing $ "Last occur: " <> tshow blLastOccur
  logger Nothing $ "Queue " ++ name ++ " checking blocks: " <> tshow blocks
  let orders = mconcat $ mapMaybe (`M.lookup` mQueues) blocks
  mapM_ (void . processCurrentMachineWip name routes) blocks
  unless (null orders) $ void $ processBlocks name True blocks
  respond (Left (nr + 1)) >>= processBlocks name True . return
  return ()
-- received an order, store and continue
queue name routes (Right order) = do
  let q = nextBlock order
  case q of
    Queue {} -> do
      let order' = processOrder routes q order
      modify $ addOrderToQueue q order'
      logger Nothing $ tshow (lastBlock order') ++ " (" ++ name ++ ") input order " ++ tshow (orderId order')
    _ -> void $ respond (pure order) -- just push through
  request q >>= queue name routes    -- request new order and process


-- | Process blocks, by responding one order for each block and then process the new requests.
processBlocks :: (MonadLogger m, MonadState SimSim m) => Text -> Bool -> [Block] -> Proxy x' x Block Downstream m [Block]
processBlocks _ _ [] = return []
processBlocks name loop (nxtBl:bs) = do
  logger Nothing $ "Processing Queue for Block " <> tshow nxtBl
  oldQueues <- gets simOrdersQueue
  allOrders <- M.findWithDefault [] nxtBl <$> gets simOrdersQueue
  -- update stats
  let minOrders = map (L.minimumBy (compare `on` blockStartTime)) $ groupBy ((==) `on` lastBlock) $ sortBy (compare `on` lastBlock) allOrders
  blTimes <- gets (simBlockTimes . simInternal)
  unless (null allOrders) $
    modify (\sim -> foldl' (\s minOrder -> let lastBl = lastBlock minOrder
                                               blTime = max (blockStartTime minOrder) (M.findWithDefault 0 lastBl blTimes)
                                           in setBlockTime lastBl blTime $ statsAddBlock ProcTime lastBl minOrder s) sim minOrders)


  -- dispatch order
  mOrder <- state (getAndRemoveOrderFromQueue nxtBl)
  case mOrder of
    Nothing -> do
      logger Nothing $ "No more orders in queue " ++ tshow nxtBl
      processBlocks name loop bs
    Just o -> do
      let thisBl = lastBlock o
      endTime <- getSimEndTime
      startTime <-
        if isMachine nxtBl
          then max (orderCurrentTime o) <$> getBlockTimeM nxtBl
          else return $ orderCurrentTime o
      isFree <- isNothing . M.lookup nxtBl <$> gets simOrdersMachine
      if not isFree || startTime > endTime
        then do
          reset oldQueues -- reset queue state to prevent re-ordering of queues
          logger Nothing $ "Reset old queues for " ++ tshow thisBl ++ " and order " ++ tshow (orderId o)
          processBlocks name True bs
        else do
          let o' = setOrderCurrentTime startTime o
          logger (Just startTime) $ "Order " ++ tshow (orderId o') ++ " just left " ++ tshow thisBl ++ " (" ++ name ++ "). Order: " ++ tshow o'
          modify (setBlockTime thisBl startTime . statsAddBlock FlowTime thisBl o')
          r <- respond $ pure o'
          rs <- processBlocks name False bs
          logger (Just startTime) $ "Responses: " <> tshow (r : rs)
          if loop
            then processBlocks name True (r : rs)
            else return (r : rs)
  where
    reset oldQueues = modify (setOrderQueue oldQueues)


processOrder :: Routing -> Block -> Order -> Order
processOrder routes q order = dispatch routes q order


--
-- Queue.hs ends here
