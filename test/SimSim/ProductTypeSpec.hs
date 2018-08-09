-- ProductTypeSpec.hs ---
--
-- Filename: ProductTypeSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  8 12:03:50 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 23
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

module SimSim.ProductTypeSpec where

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
sizedProductTypes n = take maxProducts $ map Product [1..product [1..n]]


spec :: Spec
spec = return ()


--
-- ProductTypeSpec.hs ends here
