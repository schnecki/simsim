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
--     Update #: 33
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

import           ClassyPrelude

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude                    as Pipe


import           SimSim.Block
import           SimSim.Order
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Simulation
import           SimSim.Simulation.Type


import           SimSim.Runner.Block
import           SimSim.Runner.Release

sink :: (MonadIO m) => Order -> Client Block Order (StateT SimSim m) ()
sink order = do
  -- n <- lift get
  -- lift $ put $ n+1
  -- liftIO $ putStrLn $ "Sink #" ++ show n ++ ": " ++ show order
  liftIO $ print order
  request Sink >>= sink

server :: (MonadIO m) => [Order] -> Server Block Order (StateT SimSim m) ()
server [] = return ()
server (o:os) = do
  respond o
  server os


simulate :: SimSim -> [Order] -> IO ()
simulate sim incomingOrders =
  runEffect $ evalStateP sim $
    server incomingOrders >>~ release routes >~>
    -- foldl' (>~>) (release routes) -- repeat machine & queues often enough
    -- (replicate maxMs (queue routes >~> machine routes)) >~>
    sink

  where maxMs = maxMachines $ simInternal sim
        routes = simRouting sim

--
-- Runner.hs ends here
