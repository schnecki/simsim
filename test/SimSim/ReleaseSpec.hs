{-# LANGUAGE FlexibleInstances #-}
-- ReleaseSpec.hs ---
--
-- Filename: ReleaseSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 09:26:47 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 2
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

module SimSim.ReleaseSpec (spec) where

import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Order
import           SimSim.Release


instance Arbitrary (IO [Order]) where
  arbitrary = do
    xs <- arbitrary
    return $ xs


spec :: Spec
spec = return ()


--
-- ReleaseSpec.hs ends here
