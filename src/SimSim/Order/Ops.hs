{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE TypeFamilies   #-}
-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:40:44 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 138
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

module SimSim.Order.Ops
  ( generateOrdersContDueDateDistr
  , generateOrdersDiscDueDateDistr
  , generateOrdersFixedDueDateSlack
  , generateOrdersUniform
  ) where

import           ClassyPrelude
import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics
import           Statistics.Distribution
import           Statistics.Distribution.Uniform
import           System.Random.MWC

import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Simulation.Type
import           SimSim.Time

type MinimumTime = Time

generateOrdersContDueDateDistr ::
     (ContGen interarrivalTime, DiscreteGen productType, ContGen dueDateSlack) => SimSim -> interarrivalTime -> productType -> (MinimumTime, dueDateSlack) -> IO [Order]
generateOrdersContDueDateDistr sim dInterarrival dProductType (minTime, dueDateSlack) = do
  let repeatUntilTime acc = do
        interArr <- genContVar dInterarrival gen
        if acc + interArr < periodLen
          then ((acc + interArr) :) <$> repeatUntilTime (acc + interArr)
          else return []
  arrivals <- map ((+currentTime) . timeFromDouble) <$> repeatUntilTime 0
  productTypes <- map (Product . (+ 1) . (`mod` length pts)) <$> mapM (\_ -> genDiscreteVar dProductType gen) arrivals
  dueDates <- mapM (\a -> (\x -> timeFromDouble x + a + minTime) <$> genContVar dueDateSlack gen) arrivals
  return $ zipWith3 newOrder productTypes arrivals dueDates
  where
    currentTime = simCurrentTime sim
    gen = simRandGen $ simInternal sim
    periodLen = timeToDouble $ simPeriodLength sim
    pts = productTypes sim

generateOrdersDiscDueDateDistr ::
     (ContGen interarrivalTime, DiscreteGen productType, DiscreteGen dueDateSlack) => SimSim -> interarrivalTime -> productType -> (MinimumTime, dueDateSlack) -> IO [Order]
generateOrdersDiscDueDateDistr sim dInterarrival dProductType (minTime, dueDateSlack) = do
  let repeatUntilTime acc = do
        interArr <- genContVar dInterarrival gen
        if acc + interArr < periodLen
          then ((acc + interArr) :) <$> repeatUntilTime (acc + interArr)
          else return []
  arrivals <- map ((+currentTime) . timeFromDouble) <$> repeatUntilTime 0
  productTypes <- map (Product . (+ 1) . (`mod` length pts)) <$> mapM (\_ -> genDiscreteVar dProductType gen) arrivals
  dueDates <- mapM (\_ -> (\x -> x + minTime + currentTime) . Time . fromIntegral <$> genDiscreteVar dueDateSlack gen) arrivals
  return $ zipWith3 newOrder productTypes arrivals dueDates
  where
    currentTime = simCurrentTime sim
    gen = simRandGen $ simInternal sim
    periodLen = timeToDouble $ simPeriodLength sim
    pts = productTypes sim

generateOrdersFixedDueDateSlack :: (ContGen interarrivalTime, ContGen productType) => SimSim -> interarrivalTime -> productType -> Time -> IO [Order]
generateOrdersFixedDueDateSlack sim dInterarrival dProductType dueDateSlack = do
  let repeatUntilTime acc = do
        interArr <- genContVar dInterarrival gen
        if acc + interArr < periodLen
          then ((acc + interArr) :) <$> repeatUntilTime (acc + interArr)
          else return []
  arrivals <- map ((+currentTimeMinusPeriod) . timeFromDouble) <$> repeatUntilTime 0
  print currentTimeMinusPeriod
  print $ map timeToDouble arrivals
  -- productTypes <- map (Product . (+ 1) . (`mod` length pts)) <$> mapM (\_ -> genDiscreteVar dProductType gen) arrivals
  productTypes <- map (Product . (+ 1) . (`mod` length pts) . round) <$> mapM (\_ -> genContVar dProductType gen) arrivals
  let dueDates = replicate (length arrivals) (currentTime + dueDateSlack)
  return $ zipWith3 newOrder productTypes arrivals dueDates
  where
    currentTimeMinusPeriod = currentTime - timeFromDouble periodLen
    currentTime = simCurrentTime sim
    gen = simRandGen $ simInternal sim
    periodLen = timeToDouble $ simPeriodLength sim
    pts = productTypes sim

generateOrdersUniform :: SimSim -> Int -> Int -> Time -> IO [Order]
generateOrdersUniform sim minOrders maxOrders dueDateSlack = do
  nrOrders <- round <$> genContVar (uniformDistr (fromIntegral minOrders-0.4999) (fromIntegral maxOrders+0.4999)) gen
  let arrivals = replicate nrOrders currentTime
  -- productTypes <- map (Product . (+ 1) . (`mod` length pts)) <$> mapM (\_ -> genDiscreteVar dProductType gen) arrivals
  productTypes <- map (Product . round) <$> mapM (\_ -> genContVar (uniformDistr 0.5001 (fromIntegral (length pts)+0.4999)) gen) arrivals
  let dueDates = replicate (length arrivals) (currentTime + dueDateSlack)
  return $ zipWith3 newOrder productTypes arrivals dueDates
  where
    currentTime = simCurrentTime sim
    gen = simRandGen $ simInternal sim
    pts = productTypes sim


--
-- Ops.hs ends here
