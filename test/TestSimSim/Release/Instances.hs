{-# LANGUAGE FlexibleInstances #-}
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


module TestSimSim.Release.Instances where

import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Order
import           SimSim.Release


instance Arbitrary (IO [Order]) where
  arbitrary = do
    xs <- arbitrary
    return $ xs


--
-- Instances.hs ends here
