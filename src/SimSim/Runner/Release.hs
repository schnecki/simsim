{-# LANGUAGE TemplateHaskell #-}
-- Release.hs ---
--
-- Filename: Release.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:02:25 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 71
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

module SimSim.Runner.Release
    ( release

    ) where

import           ClassyPrelude


import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Class
import           Data.Monoid                ((<>))
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
import           SimSim.Runner.Util
import           SimSim.Simulation
import           SimSim.Simulation.Type
import           SimSim.Statistics
import           SimSim.Time

import           SimSim.Runner.Dispatch

-- | Takes as input the routes and a list of order to be released into the production system.
release :: (MonadLogger m, MonadIO m) => SimSim -> Routing -> [Order] -> Server Block Downstream (StateT SimSim m) ()
release sim routes [] = void $ respond (Left 1)
release sim routes (o:os) = do
  let t = simCurrentTime sim
  let o' = process routes t o
  logger Nothing $ "Release of " ++ tshow (orderId o)
  modify (statsAddRelease o')
  void $ respond $ pure o'
  release sim routes os


process :: Routing -> Time -> Order -> Order
process routes t o = dispatch routes OrderPool $ setOrderBlockStartTime t $ setOrderCurrentTime t $ setReleaseTime t o


--
-- Release.hs ends here
