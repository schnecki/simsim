-- FirstComeFirstServe.hs ---
--
-- Filename: FirstComeFirstServe.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Jul 29 09:16:42 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 9
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

module SimSim.Dispatch.FirstComeFirstServe
    ( firstComeFirstServe
    , prioritizeProducts
    ) where

import           ClassyPrelude

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.ProductType

firstComeFirstServe :: Block -> [Order] -> [Order]
firstComeFirstServe _ = sortBy (compare `on` orderCurrentTime)

prioritizeProducts :: [ProductType] -> Block -> [Order] -> [Order]
prioritizeProducts pts _ xs = concat ptsOrders ++ nonPts
  where ptsOrders = map (\pt -> filter ((== pt) . productType) xs) pts
        nonPts = filter ((`notElem` pts) . productType) xs


--
-- FirstComeFirstServe.hs ends here
