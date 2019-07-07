{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:45:35 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Sun Jul  7 12:42:28 2019 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 11
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

import           Data.Foldable
import qualified Data.Vector.Unboxed              as V
import           GHC.Prim
import           Prelude
import           System.IO.Unsafe
import qualified System.Random.MWC                as G
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.ProcessingTime.Type

import           TestSimSim.Block.Instances
import           TestSimSim.ProductType.Instances
import           TestSimSim.Time.Instances
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


instance CoArbitrary (G.Gen RealWorld) where
  coarbitrary g = variant 0 . coarbitrary (V.toList $ unsafePerformIO (G.fromSeed <$> G.save g))

-- instance Arbitrary (G.Gen IO) where
--   arbitrary = unsafePerformIO $ createSystemRandom

-- instance CoArbitrary (G.Gen IO) where
--   coarbitrary = undefined


--
-- Instances.hs ends here
