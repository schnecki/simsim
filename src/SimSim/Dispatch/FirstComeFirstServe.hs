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
--     Update #: 6
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

import           SimSim.Order.Type
import           SimSim.ProductType

firstComeFirstServe :: [Order] -> [Order]
firstComeFirstServe = sortBy (compare `on` orderCurrentTime)

prioritizeProducts :: [ProductType] -> [Order] -> [Order]
prioritizeProducts pts xs = concat ptsOrders ++ nonPts
  where ptsOrders = map (\pt -> filter ((== pt) . productType) xs) pts
        nonPts = filter ((`notElem` pts) . productType) xs


--
-- FirstComeFirstServe.hs ends here
