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
--     Update #: 276
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
import           SimSim.Runner.Util
import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type

type QueueResponse = Maybe Order


queue :: (MonadLogger m, MonadIO m) => Text -> Routing -> Downstream -> Proxy Upstream Downstream Upstream Downstream (StateT SimSim m) ()
queue name routes (Left nr) -- no more orders from upstream, process queued orders
 = do
  mQueues <- gets simOrdersQueue --  load any order
  logger Nothing $ "Queue " ++ name ++ " input Left " ++ tshow nr ++ "\tcurrent queues:" <> tshow mQueues
  blLastOccur <- gets (simBlockLastOccur . simInternal)
  let blocks = map fst $ filter ((== nr + 1) . snd) (M.toList blLastOccur)
  let orders = mconcat $ mapMaybe (`M.lookup` mQueues) blocks
  if null orders
    then void (respond $ Left (nr + 1))
    else processBlocks name True blocks >> void (respond $ Left (nr + 1))
queue name routes (Right order)      -- received an order, store and continue
 = do
  let q = nextBlock order
  case q of
    Queue {}                    -- process and add to queue list
     -> do
      let order' = processOrder routes q order
      modify $ addOrderToQueue q order'
      logger Nothing $ tshow (lastBlock order') ++ " (" ++ name ++ ") input order " ++ tshow (orderId order) 
    _ -> void $ respond (pure order) -- just push through
  request q >>= queue name routes    -- request new order and process

-- | Process blocks, by responding one order for each block and then process the new requests.
processBlocks :: (MonadLogger m, MonadState SimSim m) => Text -> Bool -> [Block] -> Proxy x' x Block Downstream m [Block]
processBlocks _ _ [] = return []
processBlocks name loop (nxtBl:bs) = do
  oldQueues <- gets simOrdersQueue
  mOrder <- state (getAndRemoveOrderFromQueue nxtBl)
  case mOrder of
    Nothing -> processBlocks name loop bs
    Just o -> do
      endTime <- getSimEndTime
      mStartTime <-
        if isMachine nxtBl
          then (\x -> Just $ max (orderCurrentTime o) x) <$> getBlockTime nxtBl
          else return $ Just $ orderCurrentTime o
      case mStartTime of
        Nothing -> reset oldQueues -- reset queue state to prevent re-ordering of queues
        Just startTime ->
          if startTime >= endTime
            then reset oldQueues   -- reset queue state to prevent re-ordering of queues
            else do
              let o' = setOrderCurrentTime startTime o
              let thisBl = lastBlock o
              logger (Just startTime) $ "Order " ++ tshow (orderId o) ++ " just left " ++ tshow thisBl
              modify (setBlockTime thisBl startTime)
              r <- respond $ pure o'
              rs <- processBlocks name False bs
              if loop
                then processBlocks name True (r : rs)
                else return (r : rs)
  where
    reset oldQueues = modify (setOrderQueue oldQueues) >> return []


processOrder :: Routing -> Block -> Order -> Order
processOrder routes q order = dispatch routes q order


--
-- Queue.hs ends here
