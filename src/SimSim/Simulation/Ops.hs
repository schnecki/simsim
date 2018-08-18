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
--     Update #: 44
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

module SimSim.Simulation.Ops where


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
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Type
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
               (SimInternal uniqueBlocks blTimes 1 maxMachines (fromProcTimes procTimes) randomNs (M.fromList topSorts) (M.fromList lastOccur))
        else error "wrong setup"
      where check = (hasSource || error "Routing must include an OrderPool!") && (all ((== 1) . length) comps || error ("At least one route has a gap!" ++ show comps))
            allBlocks = fmap snd routes <> fmap (snd . fst) routes
            uniqueBlocks = NL.fromList $ ordNub $ NL.toList allBlocks
            maxMachines = maximum (impureNonNull $ 1 : lengths)
            routeGroups = NL.groupBy ((==) `on` fst . fst) $ NL.sortBy (compare `on` fst . fst) routes
            routeGroupsWoFgi = map (NL.filter (not . isFgi . snd)) routeGroups
            lengths = max (fmap (length . filter (not . isMachine . snd)) routeGroupsWoFgi) (fmap (length . filter (not . isQueue . snd)) routeGroupsWoFgi) -- every route is one step
            blTimes = M.fromList $ zip (filter (not . isSink) (toList allBlocks)) (repeat 0)
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


setSimEndTime :: Time -> SimSim -> SimSim
setSimEndTime t sim = sim { simInternal = (simInternal sim) { simEndTime = t }}

resetStatistics :: SimSim -> SimSim
resetStatistics sim = sim {simStatistics = emptyStatistics }

setSimCurrentTime :: Time -> SimSim -> SimSim
setSimCurrentTime t sim = sim { simCurrentTime = t }


setSimBlockTime :: Block -> Time -> SimSim -> SimSim
setSimBlockTime block t sim = sim {simInternal = (simInternal sim) {simBlockTimes = newM}}
  where
    newM = M.insert block t (simBlockTimes (simInternal sim))

setNextOrderId :: OrderId -> SimSim -> SimSim
setNextOrderId nextOrderId sim = sim { simNextOrderId = nextOrderId }

getNextOrderId :: SimSim -> (OrderId, SimSim)
getNextOrderId sim = (oId, sim { simNextOrderId = oId+1 })
  where oId = simNextOrderId sim


addNextOrderId :: OrderId -> SimSim -> SimSim
addNextOrderId add sim = sim { simNextOrderId = simNextOrderId sim + add }

addOrderToOrderPool :: Order -> SimSim -> SimSim
addOrderToOrderPool o sim = sim {simOrdersOrderPool = simOrdersOrderPool sim ++ [o]}

removeOrdersFromOrderPool :: [Order] -> SimSim -> SimSim
removeOrdersFromOrderPool os sim = sim {simOrdersOrderPool = filter (`notElem` os) (simOrdersOrderPool sim)}


setOrderQueue :: M.Map Block [Order] -> SimSim -> SimSim
setOrderQueue queues sim = sim {simOrdersQueue = queues }


addOrderToQueue :: Block -> Order -> SimSim -> SimSim
addOrderToQueue bl o sim = sim {simOrdersQueue = M.insertWith (flip (<>)) (nextBlock o) [o] (simOrdersQueue sim)}
  where
    internal = simInternal sim


getAndRemoveOrderFromQueue :: Block -> SimSim -> (Maybe Order, SimSim)
getAndRemoveOrderFromQueue bl sim = maybe def f (M.lookup bl (simOrdersQueue sim))
  where
    def = (Nothing, sim)
    blTime = fromMaybe (error $ "no blocktime for " ++ show bl) $ M.lookup bl (simBlockTimes $ simInternal sim)
    dispatchSort = dispatcher (simDispatch sim)
    arrivedUntilDecision = (<= blTime) . orderCurrentTime
    f [] = def
    f xxs =
      case (dispatchSort bl (filter arrivedUntilDecision xxs), dispatchSort bl (filter (not . arrivedUntilDecision) xxs)) of
        ([], r:rs) -> (Just r, setSimBlockTime bl (orderCurrentTime r) $ sim {simOrdersQueue = M.insert bl rs (simOrdersQueue sim)})
        (x:xs, rs) -> (Just x, sim {simOrdersQueue = M.insert bl (xs ++ rs) (simOrdersQueue sim)})


addOrderToMachine :: Order -> Time -> SimSim -> SimSim
addOrderToMachine o time sim = sim { simOrdersMachine = M.insert (lastBlock o) (o,time) (simOrdersMachine sim) }

emptyOrdersMachine :: Block -> SimSim -> SimSim
emptyOrdersMachine bl sim = sim { simOrdersMachine = M.delete bl (simOrdersMachine sim)}


addOrderToFgi :: Order -> SimSim -> SimSim
addOrderToFgi o sim = sim {simOrdersFgi = simOrdersFgi sim <> [o] }


removeOrdersFromFgi :: [Order] -> SimSim -> SimSim
removeOrdersFromFgi os sim = sim {simOrdersFgi = filter (`notElem` os) (simOrdersFgi sim)}


getAndRemoveOrderFromMachine :: Block -> SimSim -> (Maybe (Order, Time), SimSim)
getAndRemoveOrderFromMachine bl sim = maybe def f (M.lookup bl (simOrdersMachine sim))
  where
    internal = simInternal sim
    def = (Nothing, sim)
    f res = (Just res, sim {simOrdersMachine = M.delete bl (simOrdersMachine sim)})


setFinishedOrders :: [Order] -> SimSim -> SimSim
setFinishedOrders xs sim = sim { simOrdersShipped = xs }

addToBlockTime :: Block -> Time -> SimSim -> SimSim
addToBlockTime block t = onBlockTime block (t+)

setBlockTime :: Block -> Time -> SimSim -> SimSim
setBlockTime block t = onBlockTime block (const t)


onBlockTime :: Block -> (Time -> Time) -> SimSim -> SimSim
onBlockTime block f s = s {simInternal = (simInternal s) {simBlockTimes = M.update (return . f) block (simBlockTimes $ simInternal s)}}


--
-- Ops.hs ends here
