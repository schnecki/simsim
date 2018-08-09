{-# LANGUAGE TupleSections #-}
-- ProcessingTimeSpec.hs ---
--
-- Filename: ProcessingTimeSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 09:45:22 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 13
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

module SimSim.ProcessingTimeSpec
  ( spec
  , sizedProcTimes
  ) where

import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.ProcessingTime.Type

import           SimSim.BlockSpec           hiding (spec)
import           SimSim.ProductTypeSpec     hiding (spec)
import           SimSim.TimeSpec            hiding (spec)


sizedProcTimes :: Gen ProcTimes
sizedProcTimes = sized $ \n -> do
  let bls = sizedBlocks n
      ms = filter isMachine bls
      pds = sizedProductTypes n
  let mkProcTimes m = do
        ts <- mapM (\p -> arbitrary >>= return . (p,)) pds
        return (m,ts)
  mapM mkProcTimes ms

spec :: Spec
spec = return ()


--
-- ProcessingTimeSpec.hs ends here
