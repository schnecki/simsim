{-# LANGUAGE FlexibleInstances #-}
-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:50:39 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 3
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


module TestSimSim.Time.Instances where

import           ClassyPrelude
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Time

instance Arbitrary Time where
  arbitrary = Time <$> arbitrary

instance Arbitrary (IO Time) where
  arbitrary = return . Time <$> arbitrary


instance CoArbitrary Time where
  coarbitrary (Time t) = variant 0 . coarbitrary t


instance CoArbitrary (IO Time) where
  coarbitrary ioT = variant 0 . coarbitrary (unsafePerformIO ioT)


--
-- Instances.hs ends here
