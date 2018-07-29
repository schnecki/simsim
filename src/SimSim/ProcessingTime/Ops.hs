-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Nov 21 11:32:14 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 22
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

module SimSim.ProcessingTime.Ops where

import           ClassyPrelude

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Class
import qualified Data.List.NonEmpty         as NL
import qualified Data.Map.Strict            as M
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Void
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
import           SimSim.Time

import           Debug.Trace


getProcessingTime :: (MonadState SimSim m) => Block -> Order -> m Time
getProcessingTime block order = do
  r <- getNextRand
  mTime <- gets (simProcessingTimes . simInternal)
  let f = do mType <- M.lookup block mTime
             M.lookup (productType order) mType
  return $
    -- trace ("block: " ++ show block)
    -- trace ("order: " ++ show order)

    fromMaybe (error "no processing time given") (f <*> Just r)


--
-- Ops.hs ends here
