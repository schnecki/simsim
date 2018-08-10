-- Ops.hs ---
--
-- Filename: Ops.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Jul 31 13:55:41 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 13
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

module SimSim.Simulation.Ops
    ( newSimSim

    ) where


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
import           SimSim.Shipment
import           SimSim.Simulation.Type
import           SimSim.Statistics
import           SimSim.Time


newSimSim :: (RandomGen g) => g -> Routes -> ProcTimes -> PeriodLength -> Release -> Dispatch -> Shipment -> SimSim
newSimSim g routesE procTimes periodLen release dispatch shipment =
  case NL.nonEmpty (filter ((/= Sink) . snd) routesE) of
    Nothing -> error "Routing cannot be empty, and must include an OrderPool! Connections to the Sink, e.g. ((Product 1, OrderPool) --> Sink), do not count."
    Just routes ->
      if check
        then SimSim
               routes
               0
               periodLen
               1
               release
               dispatch
               shipment
               mempty
               mempty
               mempty
               mempty
               mempty
               emptyStatistics
               (SimInternal uniqueBlocks mTimes 1 maxMachines (fromProcTimes procTimes) randomNs (M.fromList topSorts) (M.fromList lastOccur))
        else error "wrong setup"
      where check = (hasSource || error "Routing must include an OrderPool!") && (all ((== 1) . length) comps || error ("At least one route has a gap!" ++ show comps))
            allBlocks = fmap snd routes <> fmap (snd . fst) routes
            uniqueBlocks = NL.fromList $ ordNub $ NL.toList allBlocks
            maxMachines = maximum (impureNonNull $ 1 : lengths)
            routeGroups = NL.groupBy ((==) `on` fst . fst) $ NL.sortBy (compare `on` fst . fst) routes
            routeGroupsWoFgi = map (NL.filter (not . isFgi . snd)) routeGroups
            lengths = max (fmap (length . filter (not . isMachine . snd)) routeGroupsWoFgi) (fmap (length . filter (not . isQueue . snd)) routeGroupsWoFgi) -- every route is one step
            mTimes = M.fromList $ zip (filter isMachine $ toList allBlocks) (repeat 0)
            hasSource = OrderPool `elem` uniqueBlocks
            randomNs = NL.fromList $ randomRs (0, 1) g
            lastOccur = map (Prelude.maximum . concat . occurances) (toList allBlocks)
            occurances bl = map (maybe [] (\x -> [(bl, fst x)]) . find ((== bl) . snd) . zip [0 ..]) (map snd topSorts)
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


--
-- Ops.hs ends here
