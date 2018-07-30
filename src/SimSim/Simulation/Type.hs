{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 20:28:23 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 258
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

module SimSim.Simulation.Type where

import           ClassyPrelude
import           Data.Graph
import qualified Data.List.NonEmpty         as NL
import qualified Data.Map.Strict            as M
import qualified Prelude                    as Prelude
import           System.Random

import           SimSim.Block
import           SimSim.Dispatch
import           SimSim.Order.Type
import           SimSim.Period
import           SimSim.ProcessingTime.Type
import           SimSim.ProductType
import           SimSim.Release
import           SimSim.Routing
import           SimSim.Time

data SimSim =
  SimSim { simRouting        :: !Routing
         , simBlocks         :: !(NL.NonEmpty Block)
         , simCurrentTime    :: !Time
         , simPeriodLength   :: !PeriodLength
         , simNextOrderId    :: !OrderId
         , simRelease        :: !Release
         , simDispatch       :: !Dispatch
         , simFinishedOrders :: ![Order] -- ^ Orders which have been finished in last period.
         , simInternal       :: !SimInternal
         }


data SimInternal =
  SimInternal { simBlockTimes      :: !(M.Map Block Time)
              , simEndTime         :: !Time
              , simMaxMachines     :: !Int
              , simProcessingTimes :: !ProcessingTimes
              , simRandomNumbers   :: !(NL.NonEmpty Double)
              , simOrderPoolOrders :: ![Order]
              , simQueueOrders     :: !(M.Map Block [Order])
              , simProductRoutes   :: !(M.Map ProductType [Block])
              }


newSimSim :: (RandomGen g) => g -> Routes -> ProcTimes -> PeriodLength -> Release -> Dispatch -> SimSim
newSimSim g routesE procTimes periodLen release dispatch =
  case NL.nonEmpty (filter ((/= Sink) . snd) routesE) of
    Nothing -> error "Routing cannot be empty, and must include an OrderPool! Connections to the Sink, e.g. ((Product 1, OrderPool) --> Sink), do not count."
    Just routes ->
      if check
        then SimSim
               routes
               uniqueBlocks
               0
               periodLen
               1
               release
               dispatch
               mempty
               (SimInternal mTimes 1 maxMachines (fromProcTimes procTimes) randomNs mempty mempty (M.fromList topSorts))
        else error "wrong setup"
      where check = (hasSource || error "Routing must include an OrderPool!") && (all ((== 1) . length) comps || error ("At least one route has a gap!" ++ show comps))
            allBlocks = fmap snd routes <> fmap (snd . fst) routes
            uniqueBlocks = NL.fromList $ ordNub $ NL.toList allBlocks
            maxMachines = maximum (impureNonNull $ 1 : lengths)
            routeGroups = NL.groupBy ((==) `on` fst . fst) $ NL.sortBy (compare `on` fst . fst) routes
            lengths = max (fmap (length . NL.filter (not . isMachine . snd)) routeGroups) (fmap (length . NL.filter (not . isQueue . snd)) routeGroups) -- every route is one step
            mTimes = M.fromList $ zip (toList allBlocks) (repeat 0)
            hasSource = OrderPool `elem` uniqueBlocks
            randomNs = NL.fromList $ randomRs (0, 1) g
            -- graph representation of routes
            keys = zip [0 ..] (toList allBlocks ++ [Sink])
            toKey bl = fromMaybe (error "could not find key in newSimSim") $ find ((== bl) . snd) keys
            fromKey nr = allBlocks NL.!! nr
            graphs = fmap (mkGraphs . appendSinkConnection) routeGroups
            appendSinkConnection xs = xs <> pure ((pt, lastBlock), Sink)
              where
                ((pt, _), lastBlock) = NL.last xs
            mkGraphs = graphFromEdges . toList . fmap (\((pt, b1), b2) -> (b1, toKey b1, [toKey b2]))
            comps = fmap (components . fst3) graphs
            fst3 (x, _, _) = x
            topSorts = zipWith associateTopSorts (fmap NL.head routeGroups) (fmap (\(g, fVert, _) -> map (fst3 . fVert) (topSort g)) graphs)
            associateTopSorts ((pt, _), _) ts = (pt, ts)


setSimEndTime :: Time -> SimSim -> SimSim
setSimEndTime t sim = sim { simInternal = (simInternal sim) { simEndTime = t }}


setSimBlockTime :: Block -> Time -> SimSim -> SimSim
setSimBlockTime block t sim = sim {simInternal = (simInternal sim) {simBlockTimes = newM}}
  where
    newM = M.insert block t (simBlockTimes (simInternal sim))

updateTailRandNrs :: SimSim -> SimSim
updateTailRandNrs sim = sim {simInternal = (simInternal sim) {simRandomNumbers = newRands}}
  where
    newRands = NL.fromList $ NL.tail $ simRandomNumbers $ simInternal sim

setNextOrderId :: OrderId -> SimSim -> SimSim
setNextOrderId nextOrderId sim = sim { simNextOrderId = nextOrderId }

addNextOrderId :: OrderId -> SimSim -> SimSim
addNextOrderId add sim = sim { simNextOrderId = simNextOrderId sim + add }

addOrderToOrderPool :: Order -> SimSim -> SimSim
addOrderToOrderPool o sim = sim {simInternal = internal {simOrderPoolOrders = simOrderPoolOrders internal ++ [o]}}
  where
    internal = simInternal sim

removeOrdersFromOrderPool :: [Order] -> SimSim -> SimSim
removeOrdersFromOrderPool os sim = sim {simInternal = internal {simOrderPoolOrders = filter (`notElem` os) (simOrderPoolOrders internal)}}
  where
    internal = simInternal sim


addOrderToQueue :: Block -> Order -> SimSim -> SimSim
addOrderToQueue bl o sim = sim {simInternal = internal {simQueueOrders = M.insertWith (flip (<>)) (nextBlock o) [o] (simQueueOrders internal)}}
  where internal = simInternal sim

getAndRemoveOrderFromQueue :: Block -> SimSim -> (Maybe Order, SimSim)
getAndRemoveOrderFromQueue bl sim = maybe def f (M.lookup bl (simQueueOrders internal))
  where internal = simInternal sim
        def = (Nothing,sim)
        f [] = def
        f (x:xs) = (Just x, sim {simInternal = internal {simQueueOrders = M.insert bl xs (simQueueOrders internal)}} )


--
-- Type.hs ends here
