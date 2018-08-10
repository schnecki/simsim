-- PrioritizeProduct.hs ---
--
-- Filename: PrioritizeProduct.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Aug 10 17:53:45 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 7
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

module SimSim.Dispatch.PrioritizeProduct
    ( dispatchPrioritizeProduct
    ) where

import           ClassyPrelude

import           SimSim.Block
import           SimSim.Dispatch.Type
import           SimSim.Order.Type
import           SimSim.ProductType


dispatchPrioritizeProduct :: [ProductType] -> Dispatch
dispatchPrioritizeProduct pts = Dispatch (prioritizeProducts pts) (name pts)

prioritizeProducts :: [ProductType] -> DispatchFunction
prioritizeProducts pts _ xs = concat ptsOrders ++ nonPts
  where ptsOrders = map (\pt -> filter ((== pt) . productType) xs) pts
        nonPts = filter ((`notElem` pts) . productType) xs

name :: [ProductType] -> Text
name pts = "PrioritizeProduct" ++ tshow pts


--
-- PrioritizeProduct.hs ends here
