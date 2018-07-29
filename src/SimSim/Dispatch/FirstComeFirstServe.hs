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
--     Update #: 4
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

import           SimSim.Order.Type

firstComeFirstServe :: [Order] -> [Order]
firstComeFirstServe = id


--
-- FirstComeFirstServe.hs ends here
