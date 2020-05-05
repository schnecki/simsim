{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- Server.hs ---
--
-- Filename: Server.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jul 28 10:47:43 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 27
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

module SimSim.Runner.Server
    ( server
    , orderPoolSink
    ) where

import           ClassyPrelude
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Class
import           Data.Monoid                ((<>))
import           Data.Sequence              (replicate)
import           Data.Text                  (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude              as Pipe
import           System.Random

import           SimSim.Block
import           SimSim.Order
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Util
import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type
import           SimSim.Time


server :: (MonadLogger m, MonadIO m) => SimSim -> [Order] -> Server Block Order (StateT SimSim m) ()
server !_ [] = return ()  -- set new order id for upcoming orders
server !sim (o:os) = do
  logger Nothing $ "Server orders: " ++ tshow (map orderId (o:os))
  let !t = simCurrentTime sim
  !nr <- state getNextOrderId
  respond $ setOrderId nr $ setOrderBlockStartTime t $ setOrderCurrentTime t o
  server sim os


orderPoolSink :: (MonadIO m) => Order -> Client Block Order (StateT SimSim m) ()
orderPoolSink !order = do
  modify (addOrderToOrderPool order)
  request OrderPool >>= orderPoolSink


--
-- Server.hs ends here
