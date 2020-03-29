{-# LANGUAGE BangPatterns    #-}
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
--     Update #: 90
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
import qualified Data.List                  as L
import           Data.Ratio                 (denominator)
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
import           SimSim.Time


-- | A FGI queues orders until shipped.
fgi :: (MonadLogger m, MonadIO m) => Downstream -> Proxy Block Downstream Block Downstream (StateT SimSim m) ()
fgi (Left !nr) = do
  !shipper <- shipment <$> gets simShipment -- (partial) period done, ship orders
  !fgiOrds <- gets simOrdersFgi
  !t <- gets (simEndTime . simInternal)
  !canShip <- gets checkShip
  if canShip
    then do
      let !ships = map (setShippedTime t) $ filter (shipper t) fgiOrds
      modify (statsAddShipped fgiOrds ships . removeOrdersFromFgi ships . setFinishedOrders ships)
      let !allShipped = length ships == length fgiOrds
      when allShipped $ modify (setBlockTime FGI t)
      logger (Just t) $ "FGI: Shipped orders " <> tshow (fmap orderId ships) <> " out of FGI orders " <> tshow (fmap orderId fgiOrds)
    else
      modify (setBlockTime FGI t . statsAddShipped fgiOrds [])
  logger Nothing $ "Left " <> tshow nr <> " in FGI."
  void $ respond $ Left (nr + 1)
fgi (Right !order) = do          -- new order arrived at fgi
  case nextBlock order of
    FGI -> do
      let !order' = setOrderBlockStartTime (orderCurrentTime order) order
      modify (addOrderToFgi order' . statsAddEndProduction order')
      logger Nothing $ "Added order " <> tshow (orderId order') <> " to FGI at time " <> tshow (orderCurrentTime order)
    _ -> void $ respond (pure order)
  !nxtOrder <- request Sink
  fgi nxtOrder


checkShip :: SimSim -> Bool
checkShip !sim = case shipmentRegularity (simShipment sim) of
  ShipEndOfPeriod -> denominator (fromTime (simEndTime $ simInternal sim) / fromTime (simPeriodLength sim)) == 1
  ShipWhenStoppedAndEndOfPeriod -> True


--
-- Fgi.hs ends here
