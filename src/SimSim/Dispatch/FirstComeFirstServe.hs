{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- FirstComeFirstServe.hs ---
--
-- Filename: FirstComeFirstServe.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Jul 29 09:16:42 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 64
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

module SimSim.Dispatch.FirstComeFirstServe
    ( firstComeFirstServe
    ) where

import           ClassyPrelude

import           SimSim.Block
import           SimSim.Dispatch.Type
import           SimSim.Order.Type
import           SimSim.ProductType


firstComeFirstServe :: Dispatch
firstComeFirstServe = Dispatch firstComeFirstServe' name

name :: Text
name = "FCFS"


firstComeFirstServe' :: DispatchFunction
firstComeFirstServe' _ = sortBy (compare `on` orderCurrentTime)


--
-- FirstComeFirstServe.hs ends here
