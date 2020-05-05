{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
--     Update #: 27
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
    , ShipmentRegularity (..)
    ) where

import           ClassyPrelude
import           Control.DeepSeq

import           SimSim.Order.Type
import           SimSim.Shipment.Regularity
import           SimSim.Time


type ShipmentFun = Time -> Order -> Bool

data Shipment = Shipment
  { shipmentRegularity :: !ShipmentRegularity
  , shipment           :: !ShipmentFun
  , uniqueShipmentName :: !Text
  } deriving (Generic, NFData)

instance Eq Shipment where
  (Shipment r1 _ n1) == (Shipment r2 _ n2) = r1 == r2 && n1 == n2

instance Ord Shipment where
  compare (Shipment r1 _ n1) (Shipment r2 _ n2) = compare (r1,n1) (r2,n2)


instance Show Shipment where
  show = unpack . uniqueShipmentName


--
-- Type.hs ends here
