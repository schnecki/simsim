-- OnDueDate.hs ---
--
-- Filename: OnDueDate.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Jul 31 15:37:43 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 19
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

module SimSim.Shipment.OnDueDate
    ( shipOnDueDate
    ) where

import           ClassyPrelude

import           SimSim.Order.Type
import           SimSim.Shipment.Regularity
import           SimSim.Shipment.Type
import           SimSim.Time

shipOnDueDate :: Shipment
shipOnDueDate = Shipment ShipEndOfPeriod shipOnDueDate' name

shipOnDueDate' :: Time -> Order -> Bool
shipOnDueDate' t = (<= t) . dueDate

name :: Text
name = "ShipOnDueDate"


--
-- OnDueDate.hs ends here
