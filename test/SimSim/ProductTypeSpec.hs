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

module SimSim.ProductTypeSpec where

import           ClassyPrelude
import           SimSim.ProductType
import           Test.Hspec
import           Test.QuickCheck

instance Arbitrary ProductType where
  arbitrary = sized $ \n -> return $ Product (n+1)
instance CoArbitrary ProductType where
  coarbitrary (Product x) = variant 0 . coarbitrary x


spec :: Spec
spec = return ()


--
-- ProductTypeSpec.hs ends here
