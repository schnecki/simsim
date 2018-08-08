-- OrderSpec.hs ---
--
-- Filename: OrderSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  8 12:00:36 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 34
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

module SimSim.OrderSpec where

import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Order.Type

import           SimSim.BlockSpec       hiding (spec)
import           SimSim.ProductTypeSpec hiding (spec)
import           SimSim.TimeSpec        hiding (spec)


fixTimes :: Order -> Order
fixTimes o@(Order _ _ _ _ rel pS pE sh _ _ _) =
  o {prodStart = bind [rel, pS], prodEnd = bind [rel, pS, pE], shipped = bind [rel, pS, pE, sh]}
  where
    bind xs = last <$> sequence xs

instance Arbitrary Order where
  arbitrary =
    fixTimes <$>
    (Order <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*>
     arbitrary <*>
     arbitrary <*>
     arbitrary <*>
     arbitrary)
instance CoArbitrary Order where
  coarbitrary (Order oid pt aD dD rT psT peT shT lB nB cT) =
    variant 0 . coarbitrary (oid, pt, aD, (dD, rT, psT, (peT, shT, lB, nB, cT)))


spec :: Spec
spec = do
  it "prop_newOrderNoTimes" $ once $ do
    newOrd <- newOrder <$> arbitrary <*> arbitrary <*> arbitrary
    return $ prop_newOrderNoTimes newOrd
  it "prop_orderFinishedAndTardy" $ property prop_orderFinishedAndTardy

prop_newOrderNoTimes :: Order -> Bool
prop_newOrderNoTimes o = and $ map (\f -> isNothing $ f o) [ released, prodStart, prodEnd, shipped]

prop_orderFinishedAndTardy :: Order -> Bool
prop_orderFinishedAndTardy o@(Order _ _ _ due _ _ pE _ _ _ _) | isJust pE = (fromJust pE > due) == orderFinishedAndTardy o
                                                              | otherwise = not (orderFinishedAndTardy o)


--
-- OrderSpec.hs ends here
