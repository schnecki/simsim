-- SimulationSpec.hs ---
--
-- Filename: SimulationSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:54:33 2018 (+0200)
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


module TestSimSim.Simulation.SimulationSpec (spec) where

import qualified Data.Map.Strict                     as M
import           Prelude
import           System.IO.Unsafe
import           System.Random
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type

import           TestSimSim.Order.Instances
import           TestSimSim.ProcessingTime.Instances
import           TestSimSim.Release.Instances
import           TestSimSim.Routing.Instances
import           TestSimSim.Statistics.Instances
import           TestSimSim.Time.Instances

import           TestSimSim.Simulation.Sim1


spec :: Spec
spec = describe "Simulation runs" $ do
  it "prop_simulation1" $ prop_simulation1


--
-- SimulationSpec.hs ends here
