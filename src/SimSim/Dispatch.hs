-- Dispatch.hs ---
--
-- Filename: Dispatch.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Jul 29 09:14:44 2018 (+0200)
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

module SimSim.Dispatch
    ( module FCFS
    , Dispatch
    ) where

import           ClassyPrelude

import           SimSim.Dispatch.FirstComeFirstServe as FCFS
import           SimSim.Order.Type

-- | Sorts the orders. The first ones will be released.
type Dispatch = [Order] -> [Order]


--
-- Dispatch.hs ends here
