-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:58:17 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 4
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


module TestSimSim.Order.Instances where

import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Order.Type
import           SimSim.ProductType

import           TestSimSim.Block.Instances
import           TestSimSim.ProductType.Instances
import           TestSimSim.Time.Instances


fixTimes :: Order -> Order
fixTimes o@(Order _ _ _ _ rel pS pE sh _ _ _ _) =
  o {prodStart = bind [rel, pS], prodEnd = bind [rel, pS, pE], shipped = bind [rel, pS, pE, sh]}
  where
    bind xs = last <$> sequence xs

instance Arbitrary Order where
  arbitrary = sized $ \n -> do
    arrDt <- arbitrary
    fixTimes <$>
      (Order <$> arbitrary <*> (Product <$> choose (1, n)) <*> pure arrDt <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> pure arrDt <*> arbitrary <*>
       arbitrary)
instance CoArbitrary Order where
  coarbitrary (Order oid pt aD dD rT psT peT shT lB lastStart nB cT) =
    variant 0 . coarbitrary (oid, pt, aD, (dD, rT, psT, (peT, shT, lB, (lastStart, nB, cT))))


--
-- Instances.hs ends here
