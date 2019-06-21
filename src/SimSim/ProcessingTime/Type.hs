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
--     Update #: 49
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
import           Control.Monad.Trans.State
import           Data.Dynamic
import qualified Data.Map.Strict           as M
import           GHC.Read
import           Pipes
import           System.Random

import           SimSim.Block
import           SimSim.Order
import           SimSim.ProductType
import           SimSim.Time


type RandomUniform = Double     -- ^ Random number between 0 and 1.

-- TODO decide on how to use random numbers
type ProcessingTime = RandomUniform -> Time

instance Eq ProcessingTime where
  _ == _ = True


type ProcessingTimes = M.Map Block (M.Map ProductType ProcessingTime)

type ProcTimes = [(Block, [(ProductType, ProcessingTime)])]


fromProcTimes :: ProcTimes -> ProcessingTimes
fromProcTimes xs = M.fromList $ fmap (second M.fromList) xs

--
-- ProcessingTime.hs ends here
