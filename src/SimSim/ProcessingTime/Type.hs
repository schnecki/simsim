{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- ProcessingTime.hs ---
--
-- Filename: ProcessingTime.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Nov 21 10:16:08 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 69
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

module SimSim.ProcessingTime.Type where

import           ClassyPrelude
import qualified Data.Map.Strict                     as M
import           Statistics.Distribution
import           Statistics.Distribution.Exponential
import           System.Random.MWC

import           SimSim.Block
import           SimSim.ProductType
import           SimSim.Time

-- ^ Use for instance Statistics.Distribution.Exponential and Statistics.Distribution to generate random numbers.
type ProcessingTime = GenIO -> IO Time

type ProcessingTimes = M.Map Block (M.Map ProductType ProcessingTime)

type ProcTimes = [(Block, [(ProductType, ProcessingTime)])]


fromProcTimes :: ProcTimes -> ProcessingTimes
fromProcTimes xs = M.fromList $ fmap (second M.fromList) xs


-- d :: ExponentialDistribution
-- d = exponential (1/0.8)

-- test :: IO [Double]
-- test = do
--   g <- createSystemRandom
--   mapM (\_ -> genContVar d g) [1..7000]


--
-- ProcessingTime.hs ends here
