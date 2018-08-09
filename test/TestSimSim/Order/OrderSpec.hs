-- OrderSpec.hs ---
--
-- Filename: OrderSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:52:56 2018 (+0200)
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


module TestSimSim.Order.OrderSpec where

import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Order.Type

import           TestSimSim.Block.Instances
import           TestSimSim.Order.Instances
import           TestSimSim.ProductType.Instances
import           TestSimSim.Time.Instances


spec :: Spec
spec = do
  describe "Order getter and setter properties" $ do
    it "prop_newOrderNoTimes" $
      once $ do
        newOrd <- newOrder <$> arbitrary <*> arbitrary <*> arbitrary
        return $ prop_newOrderNoTimes newOrd
    it "prop_orderFinishedAndTardy" $ property prop_orderFinishedAndTardy

prop_newOrderNoTimes :: Order -> Bool
prop_newOrderNoTimes o = and $ map (\f -> isNothing $ f o) [released, prodStart, prodEnd, shipped]

prop_orderFinishedAndTardy :: Order -> Bool
prop_orderFinishedAndTardy o@(Order _ _ _ due _ _ pE _ _ _ _ _)
  | isJust pE = (fromJust pE > due) == orderFinishedAndTardy o
  | otherwise = not (orderFinishedAndTardy o)


--
-- OrderSpec.hs ends here
