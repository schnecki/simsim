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
    it "prop_orderFinishedAndTardyProduction" $ property prop_orderFinishedAndTardyProduction
    it "prop_orderFinishedAndTardyShipped" $ property prop_orderFinishedAndTardyShipped

prop_newOrderNoTimes :: Order -> Bool
prop_newOrderNoTimes o = and $ map (\f -> isNothing $ f o) [released, prodStart, prodEnd, shipped]

prop_orderFinishedAndTardyProduction :: Order -> Bool
prop_orderFinishedAndTardyProduction o@(Order _ _ _ due _ _ pE _ _ _ _ _)
  | isJust pE = (fromJust pE > due) == orderFinishedAndTardyProduction o
  | otherwise = not (orderFinishedAndTardyProduction o)


prop_orderFinishedAndTardyShipped :: Order -> Bool
prop_orderFinishedAndTardyShipped o@(Order _ _ _ due _ _ _ s _ _ _ _)
  | isJust s = (fromJust s > due) == orderFinishedAndTardyShipped o
  | otherwise = not (orderFinishedAndTardyShipped o)


--
-- OrderSpec.hs ends here
