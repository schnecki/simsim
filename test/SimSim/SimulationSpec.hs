-- SimulationSpec.hs ---
--
-- Filename: SimulationSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 08:48:03 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 18
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

module SimSim.SimulationSpec (spec) where

import qualified Data.Map.Strict           as M
import           Prelude
import           System.IO.Unsafe
import           System.Random
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type

import           SimSim.OrderSpec          hiding (spec)
import           SimSim.ProcessingTimeSpec hiding (spec)
import           SimSim.ReleaseSpec        hiding (spec)
import           SimSim.RoutingSpec        hiding (spec)
import           SimSim.StatisticsSpec     hiding (spec)
import           SimSim.TimeSpec           hiding (spec)


instance Arbitrary SimSim where
  arbitrary = do
    let g = unsafePerformIO $ newStdGen
    routes <- sizedRoutes
    procTimes <- sizedProcTimes
    newSimSim g routes <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary


spec :: Spec
spec = return ()

--
-- SimulationSpec.hs ends here
