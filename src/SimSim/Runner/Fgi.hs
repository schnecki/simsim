{-# LANGUAGE TemplateHaskell #-}
-- Fgi.hs ---
--
-- Filename: Fgi.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jul 28 14:03:18 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 63
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

module SimSim.Runner.Fgi
    ( fgi
    ) where


import           ClassyPrelude

import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict hiding (mapM_)
import           Control.Monad.Trans.Class
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
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Dispatch
import           SimSim.Runner.Util
import           SimSim.Shipment
import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type
import           SimSim.Statistics


-- | A FGI queues orders until shipped.
fgi :: (MonadLogger m, MonadIO m) => Downstream -> Proxy Block Downstream Block Downstream (StateT SimSim m) ()
fgi (Left nr) = do              -- period done, ship orders
  shipper <- shipment <$> gets simShipment
  os <- gets simOrdersFgi
  t <- gets (simEndTime . simInternal)
  modify (setBlockTime FGI t)
  let ships = map (setShippedTime t) $ filter (shipper t) os
  modify (removeOrdersFromFgi ships . setFinishedOrders ships)
  mapM_ (modify . statsAddShipped) ships
  logger Nothing $ "Left " <> tshow nr <> " in FGI. Shipped orders: " <> tshow (fmap orderId ships)
  void $ respond $ Left (nr + 1)
fgi (Right order) = do          -- new order arrived at fgi
  case nextBlock order of
    FGI -> do
      let order' = setOrderBlockStartTime (orderCurrentTime order) order
      modify (addOrderToFgi order' . statsAddEndProduction order')
      logger Nothing $ "Added order " <> tshow (orderId order') <> " to FGI at time " <> tshow (orderCurrentTime order)
    _ -> void $ respond (pure order)
  nxtOrder <- request Sink
  fgi nxtOrder


--
-- Fgi.hs ends here
