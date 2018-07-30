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
--     Update #: 16
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
import           SimSim.Simulation
import           SimSim.Simulation.Type
import           SimSim.Time


server :: (MonadLogger m, MonadIO m) => SimSim -> OrderId -> [Order] -> Server Block Order (StateT SimSim m) ()
server _ nr [] = modify (addNextOrderId nr) -- set new order id for upcoming orders
server sim nr (o:os) = do
  let t = simCurrentTime sim
  respond $ setOrderId nr $ setOrderCurrentTime t o
  server sim (nr+1) os


orderPoolSink :: (MonadIO m) => Order -> Client Block Order (StateT SimSim m) ()
orderPoolSink order = do
  modify (addOrderToOrderPool order)
  request OrderPool >>= orderPoolSink


--
-- Server.hs ends here
