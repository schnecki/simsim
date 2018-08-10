-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 13:53:30 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 8
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


module TestSimSim.Simulation.Instances where

import qualified Data.Map.Strict                     as M
import           Prelude
import           System.IO.Unsafe
import           System.Random
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type
import           SimSim.Time

import           TestSimSim.Dispatch.Instances
import           TestSimSim.Order.Instances
import           TestSimSim.ProcessingTime.Instances
import           TestSimSim.Release.Instances
import           TestSimSim.Routing.Instances
import           TestSimSim.Statistics.Instances
import           TestSimSim.Time.Instances

maxPer :: Double
maxPer = 10^8

instance Arbitrary SimSim where
  arbitrary = do
    let g = unsafePerformIO $ newStdGen
    routes <- sizedRoutes
    procTimes <- sizedProcTimes
    pLen <- (Time . toRational) <$> choose (0,maxPer)
    newSimSim g routes procTimes pLen <$> arbitrary <*> arbitrary <*> arbitrary


--
-- Instances.hs ends here
