-- Sink.hs ---
--
-- Filename: Sink.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jul 28 10:48:47 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 2
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

module SimSim.Runner.Sink
    ( sink
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


sink :: (MonadIO m) => Order -> Client Block Order (StateT SimSim m) ()
sink order = do
  liftIO $ print order
  request Sink >>= sink


--
-- Sink.hs ends here
