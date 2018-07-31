-- Shipment.hs ---
--
-- Filename: Shipment.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Jul 31 15:35:04 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated: Tue Jul 31 16:04:00 2018 (+0200)
--           By: Manuel Schneckenreither
--     Update #: 15
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

module SimSim.Shipment
    ( module SimSim.Shipment.OnDueDate
    , Shipment
    ) where

import           SimSim.Shipment.OnDueDate

import           ClassyPrelude
import           SimSim.Order.Type
import           SimSim.Time

-- | Specifies whether or not the order shall be shipped.
type Shipment = Time -> Order -> Bool

--
-- Shipment.hs ends here
