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
--     Update #: 42
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

-- | The message type is a record of user name, content and timestamp. Equality is defined by the orderId, that means
-- right after creating an order with @newOrder@ all orders are equal until fed into the simulation.
data Order = Order
  { orderId          :: !OrderId
  , productType      :: !ProductType
  , arrivalDate      :: !ArrivalDate
  , dueDate          :: !DueDate
  , released         :: !(Maybe Time)
  , prodStart        :: !(Maybe Time)
  , prodEnd          :: !(Maybe Time)
  , sent             :: !(Maybe Time)
  , lastBlock        :: !Block
  , nextBlock        :: !Block
  , orderCurrentTime :: Time
  } deriving (Show)

instance Eq Order where
  x == y = orderId x == orderId y


-- | This function creates a new order.
newOrder :: ProductType -> ArrivalDate -> DueDate -> Order
newOrder productType arrDate dueDate =
  Order (-1) productType  arrDate dueDate Nothing Nothing Nothing Nothing OrderPool OrderPool 0


setOrderId :: OrderId -> Order -> Order
setOrderId nr o = o {orderId = nr}

setOrderCurrentTime :: Time -> Order -> Order
setOrderCurrentTime t o = o {orderCurrentTime = t}

addToOrderCurrentTime :: Time -> Order -> Order
addToOrderCurrentTime t o = o {orderCurrentTime = orderCurrentTime o + t}

setReleaseTime :: Time -> Order -> Order
setReleaseTime t o = o {released = Just t}

setProdStartTime :: Time -> Order -> Order
setProdStartTime t o =
  if isNothing (prodStart o)
    then o {prodStart = Just t}
    else o

setProdEndTime :: Time -> Order -> Order
setProdEndTime t o =
  if isNothing (prodEnd o)
    then o {prodEnd = Just t}
    else o

setLastBlock :: Block -> Order -> Order
setLastBlock b o = o {lastBlock = b}

setNextBlock :: Block -> Order -> Order
setNextBlock b o = o {nextBlock = b}


--
-- Type.hs ends here
