{-# LANGUAGE RankNTypes #-}
-- Release.hs ---
--
-- Filename: Release.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sat Jul 28 11:31:45 2018 (+0200)
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

module SimSim.Release
    ( module ImRe
    , Release
    ) where

import           ClassyPrelude

import           SimSim.Order
import           SimSim.Release.ImmediateRelease as ImRe
import           SimSim.Time


type Release = Time -> [Order] -> IO [Order]


--
-- Release.hs ends here
