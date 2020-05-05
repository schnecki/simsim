{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:46:52 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 10
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


module TestSimSim.Release.Instances where

import           ClassyPrelude
import           System.IO.Unsafe
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Order
import           SimSim.Release
import           SimSim.Time

import           TestSimSim.Block.Instances
import           TestSimSim.Order.Instances
import           TestSimSim.Time.Instances


instance Arbitrary (IO [Order]) where
  arbitrary = do
    xs <- arbitrary
    return $ xs
instance CoArbitrary (IO [Order]) where
  coarbitrary xIO = let xs = unsafePerformIO xIO
                    in variant 0 . coarbitrary xs

instance Arbitrary Release where
  arbitrary = Release <$> arbitrary <*> (pack <$> arbitrary)

instance CoArbitrary Release where
  coarbitrary (Release a b) = variant 0 . coarbitrary (a, unpack b)


--
-- Instances.hs ends here
