{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Strict            #-}
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
--     Update #: 94
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
import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics
import           Statistics.Distribution
import           System.Random.MWC

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
  , shipped          :: !(Maybe Time)
  , lastBlock        :: !Block
  , blockStartTime   :: !Time
  , nextBlock        :: !Block
  , orderCurrentTime :: !Time
  } deriving (Ord, Show, Generic, Serialize)

instance NFData Order where
  rnf (Order i tp ar dd rel st end sh last blSt nBl t) =
    rnf i `seq` rnf tp `seq` rnf ar `seq` rnf dd `seq` rnf1 rel `seq` rnf1 st `seq` rnf1 end `seq` rnf1 sh `seq` rnf last `seq` rnf blSt `seq` rnf nBl `seq` rnf t


instance Eq Order where
  x == y = orderId x == orderId y


-- | This function creates a new order.
newOrder :: ProductType -> ArrivalDate -> DueDate -> Order
newOrder productType arrDate dueDate =
  Order (-1) productType arrDate dueDate Nothing Nothing Nothing Nothing OrderPool arrDate OrderPool 0

orderSlackTime :: Order -> Time
orderSlackTime order = dueDate order - arrivalDate order

orderFgiTime :: Order -> Maybe Time
orderFgiTime order = (-) <$> shipped order <*> prodEnd order

orderBoTime :: Order -> Maybe Time
orderBoTime order = max 0 <$> ((-) <$> shipped order <*> Just (dueDate order))

orderWipTime :: Order -> Maybe Time
orderWipTime order = (-) <$> prodEnd order <*> prodStart order

orderFinishedProduction :: Order -> Bool
orderFinishedProduction order = isJust (prodEnd order)

orderFinishedAndTardyProduction :: Order -> Bool
orderFinishedAndTardyProduction order = maybe False (> dueDate order) (prodEnd order)

orderFinishedAndTardyShipped :: Order -> Bool
orderFinishedAndTardyShipped order = maybe False (> dueDate order) (shipped order)

orderTardinessProduction :: Order -> Maybe Time
orderTardinessProduction order = packTardy . subtract (dueDate order) =<< prodEnd order
  where packTardy x | x > 0 = return x
                    | otherwise = Nothing -- "not tardy"

orderTardinessShipped :: Order -> Maybe Time
orderTardinessShipped order = packTardy . subtract (dueDate order) =<< shipped order
  where packTardy x | x > 0 = return x
                    | otherwise = Nothing -- "not shipped"


setOrderId :: OrderId -> Order -> Order
setOrderId nr o = o {orderId = nr}

setOrderCurrentTime :: Time -> Order -> Order
setOrderCurrentTime t o = o {orderCurrentTime = t}

setOrderBlockStartTime :: Time -> Order -> Order
setOrderBlockStartTime t o = o { blockStartTime = t}


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


setShippedTime :: Time -> Order -> Order
setShippedTime t o = o {shipped = Just t}

--
-- Type.hs ends here
