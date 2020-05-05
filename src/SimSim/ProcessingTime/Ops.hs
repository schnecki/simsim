{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
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
--     Update #: 38
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

import           Control.Monad.State.Strict
import qualified Data.Map.Strict            as M
import           System.Random.MWC

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Simulation.Type
import           SimSim.Time


getProcessingTime :: (MonadIO m, MonadState SimSim m) => Block -> Order -> m Time
getProcessingTime block order = do
  g <- gets (simRandGenProcTimes . simInternal)
  mTime <- gets (simProcessingTimes . simInternal)
  let f = do
        mType <- M.lookup block mTime
        M.lookup (productType order) mType
  liftIO $ fromMaybe (error $ "no processing time specified for " ++ show (productType order) ++ " on " ++ show block) (f <*> Just g)


--
-- Ops.hs ends here
