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
--     Update #: 12
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

import           SimSim.Block
import           SimSim.Dispatch.FirstComeFirstServe as FCFS
import           SimSim.Order.Type

-- | Shall sort the orders in ascending order. Thus the first ones will be dispatched first.
type Dispatch = Block -> [Order] -> [Order]


--
-- Dispatch.hs ends here
