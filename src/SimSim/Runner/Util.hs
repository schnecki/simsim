{-# LANGUAGE TemplateHaskell #-}
-- Util.hs ---
--
-- Filename: Util.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Nov 21 11:19:38 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 61
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

module SimSim.Runner.Util where

import           ClassyPrelude

import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Class
import qualified Data.List.NonEmpty         as NL
import qualified Data.Map.Strict            as M
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import           Data.Void
import           Debug.Trace
import           qualified Prelude as Prelude
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
import           SimSim.Simulation.Type
import           SimSim.Time


type Downstream = Either Int Order
type Upstream = Block

-- | Allow logging in a proxy
instance (MonadLogger m) => MonadLogger (Proxy a b c d m) where


getSimEndTime :: (MonadState SimSim m) => m Time
getSimEndTime = gets (simEndTime . simInternal)


getNextRand :: (MonadState SimSim m) => m Double
getNextRand = do
  sim <- get
  put $ updateTailRandNrs sim
  return $ NL.head $ simRandomNumbers $ simInternal sim

getBlockTime :: (MonadState SimSim m) => Block -> m Time
getBlockTime Sink = error "called block time for a sink"
getBlockTime block = do
  m <- gets (simBlockTimes . simInternal)
  return $ fromMaybe (error $ "no block time for block: " ++ show block) (M.lookup block m)

addToBlockTime :: (MonadState SimSim m) => Block -> Time -> m ()
addToBlockTime block t = onBlockTime block (t+)

setBlockTime :: (MonadState SimSim m) => Block -> Time -> m ()
setBlockTime block t = onBlockTime block (const t)


onBlockTime :: (MonadState SimSim m) => Block -> (Time -> Time) -> m () 
onBlockTime block f = modify (\s -> s {simInternal = (simInternal s) {simBlockTimes = M.update (return . f) block (simBlockTimes $ simInternal s)}})

mapBlockTimes :: (Time -> Time) -> SimSim -> SimSim
mapBlockTimes f s = s {simInternal = (simInternal s) {simBlockTimes = M.map f (simBlockTimes $ simInternal s)}}


logger :: (MonadLogger m, MonadState SimSim m) => Maybe Time -> Text -> Proxy a b c d m ()
logger Nothing txt = do
  m <- gets (simBlockTimes . simInternal)
  let t = Prelude.maximum (M.elems m)
  logger (Just t) txt
logger (Just t) txt = $(logDebug) $ "Time " ++ tshow t ++ ": " ++ txt


--
-- Util.hs ends here
