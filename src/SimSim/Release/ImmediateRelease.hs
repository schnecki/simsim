-- ImmediateRelease.hs ---
--
-- Filename: ImmediateRelease.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jul 28 11:29:52 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 10
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

module SimSim.Release.ImmediateRelease
    ( releaseImmediate
    ) where

import           ClassyPrelude
import qualified Data.List.NonEmpty         as NL
import qualified Data.Map.Strict            as M
import           System.Random

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Period
import           SimSim.ProcessingTime.Type
import           SimSim.ProductType
import           SimSim.Release.Type
import           SimSim.Routing
import           SimSim.Time


releaseImmediate :: Release
releaseImmediate = Release immediateRelease' name

immediateRelease' :: Time -> [Order] -> IO [Order]
immediateRelease' _ = return

name :: Text
name = "ImmediateRelease"


--
-- ImmediateRelease.hs ends here
