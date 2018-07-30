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
--     Update #: 24
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

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict
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
import           SimSim.Simulation.Type


-- | A FGI queues orders until shipped.
fgi :: (MonadLogger m, MonadIO m) => Downstream -> Proxy Block Downstream Block Downstream (StateT SimSim m) ()
fgi (Left nr) = void $ respond $ Left (nr+1)
fgi (Right order) = do
  case nextBlock order of
    FGI -> do modify (addOrderToFGI order)
              $(logDebug) $ "Added order " <> tshow (orderId order) <> " to FGI at time " <> tshow (orderCurrentTime order)
    _   -> void $ respond (pure order)
  nxtOrder <- request Sink
  fgi nxtOrder


--
-- Fgi.hs ends here
