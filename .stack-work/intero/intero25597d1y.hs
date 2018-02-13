-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:40:44 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 20
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

module SimSim.Order.Type where

import           ClassyPrelude

import           SimSim.Block
import           SimSim.ProductType
import           SimSim.Time


type OrderId = Integer

-- | The message type is a record of user name, content and timestamp.
data Order = Order { orderId     :: !OrderId
                   , productType :: !ProductType
                   , arrivalDate :: !ArrivalDate
                   , dueDate     :: !DueDate
                   , released    :: !(Maybe Time)
                   , prodStart   :: !(Maybe Time)
                   , prodEnd     :: !(Maybe Time)
                   , sent        :: !(Maybe Time)
                   , lastMachine :: !Block
                   , nextMachine :: !Block
                   } deriving (Show)


-- | This function creates a new order.
newOrder :: ProductType -> ArrivalDate -> DueDate -> Order
newOrder productType arrDate dueDate =
  Order (-1) productType  arrDate dueDate Nothing Nothing Nothing Nothing Source Source


setOrderId :: OrderId -> Order -> Order
setOrderId nr o = o { orderId = nr }


--
-- Type.hs ends here
