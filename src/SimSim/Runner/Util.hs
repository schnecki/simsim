{-# LANGUAGE BangPatterns #-}
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
--     Update #: 76
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
import Data.Ratio (denominator)
import           Debug.Trace
import qualified Prelude
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude              as Pipe
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen

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

-- updateTailRandNrs :: SimSim -> SimSim
-- updateTailRandNrs sim = sim {simInternal = (simInternal sim) {simRandomNumbers = newRands}}
--   where
--     newRands = NL.fromList $ NL.tail $ simRandomNumbers $ simInternal sim
-- getNextRand :: (MonadState SimSim m) => m Double
-- getNextRand = do
--   sim <- get
--   put $ updateTailRandNrs sim
--   return $ NL.head $ simRandomNumbers $ simInternal sim

getBlockTimeM :: (MonadState SimSim m) => Block -> m Time
getBlockTimeM = gets . getBlockTime

getBlockTime :: Block -> SimSim -> Time
getBlockTime Sink _ = error "called block time for a sink"
getBlockTime !block !sim = fromMaybe (error $ "no block time for block: " ++ show block) (M.lookup block m)
  where
    m = simBlockTimes $ simInternal sim


mapBlockTimes :: (Time -> Time) -> SimSim -> SimSim
mapBlockTimes !f !s = s {simInternal = (simInternal s) {simBlockTimes = M.map f (simBlockTimes $ simInternal s)}}


logger :: (MonadLogger m, MonadState SimSim m) => Maybe Time -> Text -> Proxy a b c d m ()
logger !Nothing !txt = do
  m <- gets (simBlockTimes . simInternal)
  let t = Prelude.maximum (M.elems m)
  logger (Just t) txt
logger (Just t) txt = $(logDebug) $ "t=" ++ tshow (pretty t) ++ ": " ++ txt


isPeriodEnd :: SimSim -> Bool
isPeriodEnd !sim
  | simCurrentTime sim /= 0 && denominator (fromTime (simCurrentTime sim) / fromTime (simPeriodLength sim)) == 1 = True
  | otherwise = False


--
-- Util.hs ends here
