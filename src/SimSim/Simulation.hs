-- Simulation.hs ---
--
-- Filename: Simulation.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 16:42:32 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 15
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

module SimSim.Simulation
  ( module SimSim.Simulation.Type
  , module SimSim.Simulation.Ops
  ) where


import           SimSim.Simulation.Ops  (newSimSim)
import           SimSim.Simulation.Type (SimSim (..), resetStatistics, setSimEndTime)

--
-- Simulation.hs ends here
