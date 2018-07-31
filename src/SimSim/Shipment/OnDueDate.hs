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
--     Update #: 10
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
import           SimSim.Time

shipOnDueDate :: Time -> Order -> Bool
shipOnDueDate t = (<= t) . dueDate


--
-- OnDueDate.hs ends here
