-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:46:34 2018 (+0200)
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


module TestSimSim.ProductType.Instances where

import           Prelude
import           SimSim.ProductType
import           Test.Hspec
import           Test.QuickCheck

-- | Maximum number of products for testing. This hinders the exponential growth!
maxProducts :: Int
maxProducts = product [1..4]    -- 24

instance Arbitrary ProductType where
  arbitrary = sized $ \n -> return $ Product (n+1)
instance CoArbitrary ProductType where
  coarbitrary (Product x) = variant 0 . coarbitrary x

-- | Products to given size, where the size is the number of machines, as defined by @SimSim.BlockSpec@.
sizedProductTypes :: Int -> [ProductType]
sizedProductTypes n = take maxProducts $ map Product [1..max 1 (product [1..n])]


spec :: Spec
spec = return ()


--
-- Instances.hs ends here
