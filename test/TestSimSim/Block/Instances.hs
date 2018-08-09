-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 13:54:43 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 1
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


module TestSimSim.Block.Instances (sizedBlocks,blockSizeNr) where

import qualified Data.Map.Strict as M
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block


sizedBlocks :: Int -> [Block]
sizedBlocks 0 = [OrderPool, FGI, Sink]
sizedBlocks n = [Queue n, Machine n] ++ sizedBlocks (n-1)

blockSizeNr :: Block -> Int
blockSizeNr OrderPool   = 0
blockSizeNr FGI         = 0
blockSizeNr Sink        = 0
blockSizeNr (Queue n)   = n
blockSizeNr (Machine n) = n

instance Arbitrary Block where
  arbitrary = sized $ \n -> oneof (return <$> sizedBlocks n)
instance CoArbitrary Block where
  coarbitrary OrderPool   = variant 0 . coarbitrary (0::Int)
  coarbitrary FGI         = variant 1 . coarbitrary (1::Int)
  coarbitrary Sink        = variant 2 . coarbitrary (2::Int)
  coarbitrary (Queue n)   = variant n . coarbitrary n
  coarbitrary (Machine n) = variant (2*n) . coarbitrary (2*n)


--
-- Instances.hs ends here
