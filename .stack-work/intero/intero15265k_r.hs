-- Runner.hs ---
--
-- Filename: Runner.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:00:27 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 48
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

module SimSim.Runner.Runner
    ( simulate
    ) where

import           ClassyPrelude                    hiding (replicate)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Monoid                      ((<>))
import           Data.Sequence                    (replicate)
import           Data.Text                        (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude                    as Pipe
import           System.Random


import           SimSim.Block
import           SimSim.Order
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Simulation
import           SimSim.Simulation.Type
import           SimSim.Time


import           SimSim.Runner.Block
import           SimSim.Runner.Release

sink :: (MonadIO m) => Order -> Client Block Order (StateT SimSim m) ()
sink order = do
  -- n <- lift get
  -- lift $ put $ n+1
  -- liftIO $ putStrLn $ "Sink #" ++ show n ++ ": " ++ show order
  liftIO $ print order
  request Sink >>= sink

server :: (MonadIO m) => OrderId -> [Order] -> Server Block Order (StateT SimSim m) ()
server nr [] = lift $ modify (addNextOrderId nr)
server nr (o:os) = do
  respond $ setOrderId nr o
  server (nr+1) os


simulate :: SimSim -> [Order] -> IO ()
simulate sim incomingOrders =
  runEffect $ evalStateP sim $
    server nxtOrderId incomingOrders >>~ release routes >~>
    foldl' (>~>) (release routes) -- repeat machine & queues often enough
    (replicate maxMs (queue routes >~> machine routes)) >~>
    sink

  where maxMs = maxMachines $ simInternal sim
        routes = simRouting sim
        nxtOrderId = simNextOrderId sim

simulateUntil :: Time -> SimSim -> [Order] -> IO ()
simulateUntil endTime sim incomingOrders =
  runEffect $ evalStateP (updateSimEndTime endTime sim) $
    server nxtOrderId incomingOrders >>~ release routes >~>
    foldl' (>~>) (release routes) -- repeat machine & queues often enough
    (replicate maxMs (queue routes >~> machine routes)) >~>
    sink

  where maxMs = maxMachines $ simInternal sim
        routes = simRouting sim
        nxtOrderId = simNextOrderId sim


--
-- Runner.hs ends here
