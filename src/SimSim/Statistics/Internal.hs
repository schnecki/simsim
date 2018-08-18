-- Internal.hs ---
--
-- Filename: Internal.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 08:32:19 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 40
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


module SimSim.Statistics.Internal where

import           ClassyPrelude
import qualified Data.Map.Strict        as M

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Simulation.Type
import           SimSim.Statistics.Type
import           SimSim.Time


data Update
  = UpBlock Block               -- ^ For flow time through given block.
  | EndProd                     -- ^ For flow time through production.
  | Shipped                     -- ^ For flow time through whole system.
  deriving (Show)

-- | Updates ``StatsOrderTime`` as part of ``StatsFlowTime``.
updateStatsOrderTime :: Bool -> Update -> Order -> StatsOrderTime -> StatsOrderTime
updateStatsOrderTime isPartial up order st@(StatsOrderTime tSum stdDev _) =
  StatsOrderTime
    (tSum + getBlockFlowTime up order)
    (stdDev)
    (if isPartial
       then Just $ st {statsLastUpdatePartial = Nothing}
       else Nothing)

-- | Updates ``StatsOrderTard`` as part of ``StatsFlowTime``.
updateTardiness :: Update -> Order -> StatsOrderTard -> StatsOrderTard
updateTardiness up order st@(StatsOrderTard nr tardSum stdDev) =
  case up of
    EndProd ->
      case orderTardinessProduction order of
        Nothing -> st
        Just x  -> StatsOrderTard (nr + 1) (tardSum + fromTime x) (stdDev) -- TODO
    Shipped ->
      case orderTardinessShipped order of
        Nothing -> st
        Just x  -> StatsOrderTard (nr + 1) (tardSum + fromTime x) (stdDev) -- TODO
    _ -> st

-- | Updates the ``StatsOrderCost``.
updateCosts :: Update -> Order -> StatsOrderCost -> StatsOrderCost
updateCosts up order st@(StatsOrderCost earn wip bo fgi) =
  case up of
    Shipped -> StatsOrderCost (earn+1) wip bo fgi -- just shipped
    _       -> st


-- | Returns the flow time of an order according to the given update block sequence.
getBlockFlowTime :: Update -> Order -> Rational
getBlockFlowTime bl order =
  case bl of
    UpBlock bl ->
      case bl of
        OrderPool  -> fromTime $ fromMaybe err $ (-) <$> released order <*> pure (arrivalDate order) -- released
        FGI        -> fromTime $ fromMaybe err $ (-) <$> shipped order <*> prodEnd order             -- released
        Sink       -> error "Update of Sink not possible"
        Machine {} -> fromTime $ orderCurrentTime order - blockStartTime order
        Queue {}   -> fromTime $ orderCurrentTime order - blockStartTime order
    EndProd -> fromTime $ fromMaybe err $ (-) <$> prodEnd order <*> released order -- finished production
    Shipped -> fromTime $ fromMaybe err $ (-) <$> shipped order <*> released order -- shipped
  where
    err = error "Nothing in getBlockFlowTime"


--
-- Internal.hs ends here
