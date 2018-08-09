{-# LANGUAGE TupleSections #-}
-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:45:35 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Thu Aug  9 22:58:37 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 5
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


module TestSimSim.ProcessingTime.Instances
  ( sizedProcTimes
  ) where

import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.ProcessingTime.Type

import           TestSimSim.Block.Instances
import           TestSimSim.ProductType.Instances
import           TestSimSim.Time.Instances


sizedProcTimes :: Gen ProcTimes
sizedProcTimes = sized $ \n -> do
  let bls = sizedBlocks n
      ms = filter isMachine bls
      pds = sizedProductTypes n
  let mkProcTimes m = do
        ts <- mapM (\p -> arbitrary >>= return . (p,)) pds
        return (m,ts)
  mapM mkProcTimes ms


--
-- Instances.hs ends here
