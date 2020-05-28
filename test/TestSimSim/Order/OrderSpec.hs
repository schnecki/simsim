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
--     Update #: 46
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

import           Control.Monad
import           Data.List                        (transpose)
import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck                  hiding ((===))

import           SimSim
import           SimSim.Order.Type

import           TestSimSim.Block.Instances       ()
import           TestSimSim.Order.Instances       ()
import           TestSimSim.ProductType.Instances ()
import           TestSimSim.Simulation.Instances  ()
import           TestSimSim.Time.Instances        ()
import           TestSimSim.Util


spec :: Spec
spec =
  describe "Order getter and setter properties" $ do
    it "prop_newOrderNoTimes" $
      once $ do
        newOrd <- newOrder <$> arbitrary <*> arbitrary <*> arbitrary
        return $ prop_newOrderNoTimes newOrd
    it "prop_orderFinishedAndTardyProduction" $ property prop_orderFinishedAndTardyProduction
    it "prop_orderFinishedAndTardyShipped" $ property prop_orderFinishedAndTardyShipped
    it "prop_orderOrdersUniform checking mean orders" $ property prop_generateOrdersUniformMean
    it "prop_orderOrdersUniform min max orders" $ property prop_generateOrdersUniformMinMax
    it "prop_orderOrdersUniform mean number of product types" $ property prop_generateOrdersUniformNrProductTypes

prop_newOrderNoTimes :: Order -> Bool
prop_newOrderNoTimes o = all (\f -> isNothing $ f o) [released, prodStart, prodEnd, shipped]

prop_orderFinishedAndTardyProduction :: Order -> Bool
prop_orderFinishedAndTardyProduction o@(Order _ _ _ due _ _ pE _ _ _ _ _)
  | isJust pE = (fromJust pE > due) == orderFinishedAndTardyProduction o
  | otherwise = not (orderFinishedAndTardyProduction o)


prop_orderFinishedAndTardyShipped :: Order -> Bool
prop_orderFinishedAndTardyShipped o@(Order _ _ _ due _ _ _ s _ _ _ _)
  | isJust s = (fromJust s > due) == orderFinishedAndTardyShipped o
  | otherwise = not (orderFinishedAndTardyShipped o)

prop_generateOrdersUniformMean :: SimSim -> Int -> Int -> Property
prop_generateOrdersUniformMean sim minNr maxNr =
  minNr >= 0 && minNr <= maxNr && length (productTypes sim) < 100 ==> ioProperty $ do
    ords <- map length <$> replicateM nr (generateOrdersUniform sim minNr maxNr 10)
    let avg = fromIntegral (sum ords) / fromIntegral (length ords)
        tar = fromIntegral (minNr + maxNr) / 2
    return $ eqEps eps tar (avg :: Double)
  where
    nr = 2*10 ^ (4 :: Int)
    eps = 0.30

prop_generateOrdersUniformNrProductTypes :: SimSim -> Int -> Int -> Property
prop_generateOrdersUniformNrProductTypes sim minNr maxNr =
  minNr >= 0 && minNr <= maxNr && length (productTypes sim) < 100 ==> ioProperty $ do
    ords <- replicateM nr (generateOrdersUniform sim minNr maxNr 10)
    let pts = productTypes sim
        nrs = map avg $ transpose $ map (\os -> map (\pt -> length $ filter ((== pt) . productType) os) pts) ords
        tar :: Double
        tar = fromIntegral (minNr + maxNr) / (2 * fromIntegral (length pts))
    return $ counterexample (show (tar - eps) ++ " <= " ++ show nrs ++ " <= " ++ show (tar + eps) ++ "; pts: " ++ show pts) $ all (inRange tar) nrs
  where
    avg xs = fromIntegral (sum xs) / fromIntegral (length xs)
    nr = 2*10 ^ (4 :: Int)
    eps = 0.50
    inRange expected v = low <= v && v <= high
      where
        low = expected - eps
        high = expected + eps


prop_generateOrdersUniformMinMax :: SimSim -> Int -> Int -> Property
prop_generateOrdersUniformMinMax sim minNr maxNr =
  minNr >= 0 && minNr <= maxNr ==> ioProperty $ do
    ords <- generateOrdersUniform sim minNr maxNr 10
    return $ inRange minNr maxNr (length ords)


--
-- OrderSpec.hs ends here
