-- Dispatch.hs ---
--
-- Filename: Dispatch.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:02:47 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 32
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

module SimSim.Runner.Dispatch
    ( dispatch
    ) where

import           ClassyPrelude

import           Control.Monad
import           Control.Monad.IO.Class
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


import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Routing


dispatch :: Routing -> Block -> Order -> Order
dispatch routes lastBl order =
  case find ((== (productType order, lastBl)) . fst) routes of
    Just (_, n) -> order {lastBlock = lastBl, nextBlock = n}
    Nothing     -> order {lastBlock = lastBl, nextBlock = Sink, prodEnd = Just (orderCurrentTime order)}


--
-- Dispatch.hs ends here
