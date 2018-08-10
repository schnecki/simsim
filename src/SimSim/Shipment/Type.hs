-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Aug 10 19:15:36 2018 (+0200)
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

module SimSim.Shipment.Type
    ( Shipment (..)
    , ShipmentFun (..)
    ) where

import           ClassyPrelude

import           SimSim.Order
import           SimSim.Time

type ShipmentFun = Time -> Order -> Bool

data Shipment = Shipment
  { shipment           :: ShipmentFun
  , uniqueShipmentName :: Text
  }

instance Show Shipment where
  show = unpack . uniqueShipmentName


--
-- Type.hs ends here
