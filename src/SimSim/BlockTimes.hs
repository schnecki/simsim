-- BlockTimes.hs ---
--
-- Filename: BlockTimes.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Jun 21 12:52:38 2019 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 2
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


module SimSim.BlockTimes where

import           SimSim.Block
import           SimSim.Time

import qualified Data.Map.Strict as M

type BlockTimes = M.Map Block Time


--
-- BlockTimes.hs ends here
