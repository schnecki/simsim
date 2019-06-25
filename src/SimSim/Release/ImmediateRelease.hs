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
--     Update #: 11
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
import           SimSim.Order.Type
import           SimSim.Release.Type
import           SimSim.Time


releaseImmediate :: Release
releaseImmediate = Release immediateRelease' name

immediateRelease' :: Time -> [Order] -> IO [Order]
immediateRelease' _ = return

name :: Text
name = "ImmediateRelease"


--
-- ImmediateRelease.hs ends here
