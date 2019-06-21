-- ShipmentRegularity.hs ---
--
-- Filename: ShipmentRegularity.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Aug 21 09:10:25 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 5
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

module SimSim.Shipment.Regularity
    ( ShipmentRegularity (..)
    ) where

import           ClassyPrelude

-- | Decides on how often finished orders are shipped.
data ShipmentRegularity
  = ShipEndOfPeriod
  | ShipWhenStoppedAndEndOfPeriod
  --  | ShipEvery Rational -- need to add the step size in Runner.simulation
  deriving (Eq, Ord)


--
-- ShipmentRegularity.hs ends here
