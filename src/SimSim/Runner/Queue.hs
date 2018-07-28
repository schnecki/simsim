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
--     Update #: 44
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

import           Control.Monad
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
import           System.Random


import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Runner.Dispatch
import           SimSim.Runner.Util
import           SimSim.Simulation.Type


queue :: (MonadIO m) =>
         Routing -> Order -> Proxy Block Order Block Order (StateT SimSim m) ()
queue routes order = do
  let q = nextBlock order
  case q of
    Queue{} -> process routes q order >>= void . respond
    _       -> void $ respond order
  nxtOrder <- request q
  queue routes nxtOrder


process :: (Monad m) => Routing -> Block -> Order -> m Order
process routes q order = return $ dispatch routes $ order {lastBlock = q}

--
-- Queue.hs ends here
