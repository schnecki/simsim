{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Aug 10 17:57:15 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
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

module TestSimSim.Dispatch.Instances where

import           ClassyPrelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Dispatch.Type

import           TestSimSim.Block.Instances
import           TestSimSim.Order.Instances

instance Arbitrary Dispatch where
  arbitrary = Dispatch <$> arbitrary <*> (pack <$> arbitrary)

instance CoArbitrary Dispatch where
  coarbitrary (Dispatch a b) = variant 0 . coarbitrary (a, unpack b)


--
-- Instances.hs ends here
