-- Simulation1Sepc.hs ---
--
-- Filename: Simulation1.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug 15 18:42:51 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 126
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


module TestSimSim.Simulation.Simulation1Spec
  ( spec
  ) where

import qualified Data.List                           as L
import qualified Data.Map.Strict                     as M
import           Debug.Trace
import           Prelude
import           System.Random
import           Test.Hspec
import           Test.QuickCheck                     hiding ((===))
import           Text.PrettyPrint.ANSI.Leijen

import           SimSim
import           SimSim.Order
import           SimSim.Simulation.Ops
import           SimSim.Simulation.Pretty
import           SimSim.Simulation.Type


import           TestSimSim.Order.Instances
import           TestSimSim.ProcessingTime.Instances
import           TestSimSim.Release.Instances
import           TestSimSim.Routing.Instances
import           TestSimSim.Statistics.Instances
import           TestSimSim.Time.Instances
import           TestSimSim.Util


routing :: Routes
routing =
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> 2 -> sink
  , (Product 1, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 1, Machine 1) --> Queue 2
  , (Product 1, Queue 2)   --> Machine 2
  , (Product 1, Machine 2) --> FGI

  , (Product 2, OrderPool) --> Queue 2   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 2)   --> Machine 2 -- note: route to sink is not necessary
  , (Product 2, Machine 2) --> Queue 1
  , (Product 2, Queue 1)   --> Machine 1
  , (Product 2, Machine 1) --> FGI
  ]

periodLen :: Time
periodLen = 10

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, const 3)
                        ,(Product 2, const 2)])
            ,(Machine 2,[(Product 1, const 2)
                        ,(Product 2, const 4)])
            ]


-- | Order to send through the production
orders :: [Order]
orders = numberOrders $ L.concat $ L.replicate 1
  [ newOrder (Product 1) 0 10
  , newOrder (Product 2) 0 10
  , newOrder (Product 1) 0 10
  , newOrder (Product 1) 0 10
  ]

numberOrders :: [Order] -> [Order]
numberOrders = zipWith (\nr o -> o {orderId = nr}) [1..]

spec :: Spec
spec = do
  describe "Simulation runs" $ do
    it "prop_simulation1 Time=1" $ prop_simulation1AtTime 1 simAtTime1
    it "prop_simulation1 Time=2" $ prop_simulation1AtTime 2 simAtTime2
    it "prop_simulation1 Time=3" $ prop_simulation1AtTime 3 simAtTime3
    it "prop_simulation1 Time=4" $ prop_simulation1AtTime 4 simAtTime4
    it "prop_simulation1 Time=5" $ prop_simulation1AtTime 5 simAtTime5
    it "prop_simulation1 Time=6" $ prop_simulation1AtTime 6 simAtTime6
    it "prop_simulation1 Time=7" $ prop_simulation1AtTime 7 simAtTime7
  describe "Simulation combinations" $ do
    it "prop_simulation1 1plus1Eq2" $ prop_simulation1plus1Eq2


prop_simulation1AtTime :: Time -> (SimSim -> SimSim) -> Property
prop_simulation1AtTime t f =
  ioProperty $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    sim' <- simulateUntil t sim orders
    return $ eqPretty (f sim) (prettySimulation True False prettyOrderDue) sim' (prettySimulation True True prettyOrderDue)
    -- return $ f sim ==== sim'

prop_simulation1plus1Eq2 :: Property
prop_simulation1plus1Eq2 = ioProperty $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    sim1 <- simulateUntil 1 sim orders
    sim2 <- simulateUntil 2 sim1 []
    sim' <- simulateUntil 2 sim orders
    return $ eqPretty sim2 (prettySimulation True True prettyOrderDue) sim' (prettySimulation True True prettyOrderDue)


simAtTime1 :: SimSim -> SimSim
simAtTime1 sim =
  sim
  { simCurrentTime = 1
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, drop 2 orders), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 0, 2)), (Machine 2, (orders !! 1, 3))]
  , simOrdersFgi = []
  , simOrdersShipped = []
  , simStatistics = statsAtTime1
  }
  where
    statsAtTime1 :: SimStatistics
    statsAtTime1 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList
            [ (OrderPool, SimFlowTimeStats 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, SimFlowTimeStats 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 2, SimFlowTimeStats 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Machine 1, SimFlowTimeStats 0 (StatsOrderTime 1 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            , (Machine 2, SimFlowTimeStats 0 (StatsOrderTime 1 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            ]
      ,  simStatsBlockProcTimes =
          M.fromList [(OrderPool, StatsProcTime 0), (Queue 1, StatsProcTime 0), (Queue 2, StatsProcTime 1), (Machine 1, StatsProcTime 1), (Machine 2, StatsProcTime 1)]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0

simAtTime2 :: SimSim -> SimSim
simAtTime2 sim =
  sim
  { simCurrentTime = 2
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, drop 2 orders), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 0, 1)), (Machine 2, (orders !! 1, 2))]
  , simOrdersFgi = []
  , simOrdersShipped = []
  , simStatistics = statsAtTime1
  }
  where
    statsAtTime1 :: SimStatistics
    statsAtTime1 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList
            [ (OrderPool, SimFlowTimeStats 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, SimFlowTimeStats 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 2, SimFlowTimeStats 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Machine 1, SimFlowTimeStats 0 (StatsOrderTime 2 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            , (Machine 2, SimFlowTimeStats 0 (StatsOrderTime 2 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            ]
      ,  simStatsBlockProcTimes =
          M.fromList [(OrderPool, StatsProcTime 0), (Queue 1, StatsProcTime 0), (Queue 2, StatsProcTime 2), (Machine 1, StatsProcTime 2), (Machine 2, StatsProcTime 2)]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0


simAtTime3 :: SimSim -> SimSim
simAtTime3 sim =
  sim
  { simCurrentTime = 3
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, drop 3 orders), (Machine 2, [orders !! 0])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 2, 3)), (Machine 2, (orders !! 1, 1))]
  , simOrdersFgi = []
  , simOrdersShipped = []
  , simStatistics = statsAtTime1
  }
  where
    statsAtTime1 :: SimStatistics
    statsAtTime1 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList
            [ (OrderPool, SimFlowTimeStats 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, SimFlowTimeStats 2   (StatsOrderTime 3 0 Nothing) Nothing)
            , (Queue 2, SimFlowTimeStats 1   (StatsOrderTime 0 0 Nothing) Nothing)
            , (Machine 1, SimFlowTimeStats 1 (StatsOrderTime 3 0 Nothing) Nothing)
            , (Machine 2, SimFlowTimeStats 0 (StatsOrderTime 3 0 Nothing) Nothing)
            ]
      ,  simStatsBlockProcTimes =
          M.fromList [(OrderPool, StatsProcTime 0), (Queue 1, StatsProcTime 0), (Queue 2, StatsProcTime 3), (Machine 1, StatsProcTime 3), (Machine 2, StatsProcTime 3)]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0

simAtTime4 :: SimSim -> SimSim
simAtTime4 sim =
  sim
  { simCurrentTime = 4
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, drop 3 orders ++ [orders !! 1]), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 2, 2)), (Machine 2, (orders !! 0, 2))]
  , simOrdersFgi = []
  , simOrdersShipped = []
  , simStatistics = statsAtTime1
  }
  where
    statsAtTime1 :: SimStatistics
    statsAtTime1 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList            -- fully finished queue orders only, but full machine processing times
            [ (OrderPool, SimFlowTimeStats 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1,   SimFlowTimeStats 2 (StatsOrderTime 3 0 Nothing) Nothing)
            , (Queue 2,   SimFlowTimeStats 2 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, SimFlowTimeStats 1 (StatsOrderTime 4 0 Nothing) Nothing)
            , (Machine 2, SimFlowTimeStats 1 (StatsOrderTime 4 0 Nothing) Nothing)
            ]
      ,  simStatsBlockProcTimes =   -- processing times
          M.fromList [(OrderPool, StatsProcTime 0), (Queue 1, StatsProcTime 0), (Queue 2, StatsProcTime 3), (Machine 1, StatsProcTime 4), (Machine 2, StatsProcTime 4)]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0

simAtTime5 :: SimSim -> SimSim
simAtTime5 sim =
  sim
  { simCurrentTime = 5
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, drop 3 orders ++ [orders !! 1]), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 2, 1)), (Machine 2, (orders !! 0, 1))]
  , simOrdersFgi = []
  , simOrdersShipped = []
  , simStatistics = statsAtTime1
  }
  where
    statsAtTime1 :: SimStatistics
    statsAtTime1 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList            -- fully finished queue orders only, but full machine processing times
            [ (OrderPool, SimFlowTimeStats 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1,   SimFlowTimeStats 2 (StatsOrderTime 3 0 Nothing) Nothing)
            , (Queue 2,   SimFlowTimeStats 2 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, SimFlowTimeStats 1 (StatsOrderTime 5 0 (Just $ StatsOrderTime 3 0 Nothing)) Nothing)
            , (Machine 2, SimFlowTimeStats 1 (StatsOrderTime 5 0 (Just $ StatsOrderTime 3 0 Nothing)) Nothing)
            ]
      ,  simStatsBlockProcTimes =   -- idle times for queues, processing times for machines and orderpool
          M.fromList [(OrderPool, StatsProcTime 0), (Queue 1, StatsProcTime 0), (Queue 2, StatsProcTime 4), (Machine 1, StatsProcTime 5), (Machine 2, StatsProcTime 5)]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0


simAtTime6 :: SimSim -> SimSim
simAtTime6 sim =
  sim
  { simCurrentTime = 6
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, [orders !! 1]), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 3, 3)), (Machine 2, (orders !! 2, 2))]
  , simOrdersFgi = [orders !! 0]
  , simOrdersShipped = []
  , simStatistics = statsAtTime1
  }
  where
    statsAtTime1 :: SimStatistics
    statsAtTime1 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of  fully finished queue orders only, but full machine processing times
            [ (OrderPool, SimFlowTimeStats 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, SimFlowTimeStats 3 (StatsOrderTime 9 0 Nothing) Nothing)
            , (Queue 2, SimFlowTimeStats 3 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, SimFlowTimeStats 2 (StatsOrderTime 6 0 Nothing) Nothing)
            , (Machine 2, SimFlowTimeStats 2 (StatsOrderTime 6 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         = M.fromList [(OrderPool, StatsProcTime 0), (Queue 1, StatsProcTime 0), (Queue 2, StatsProcTime 5), (Machine 1, StatsProcTime 6), (Machine 2, StatsProcTime 6)]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          SimFlowTimeStats
          { statsNrOrders = 1
          , statsOrderFlowTime = StatsOrderTime 6 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0


simAtTime7 :: SimSim -> SimSim
simAtTime7 sim =
  sim
  { simCurrentTime = 7
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, [orders !! 1]), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 3, 2)), (Machine 2, (orders !! 2, 1))]
  , simOrdersFgi = [orders !! 0]
  , simOrdersShipped = []
  , simStatistics = statsAtTime1
  }
  where
    statsAtTime1 :: SimStatistics
    statsAtTime1 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, SimFlowTimeStats 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, SimFlowTimeStats 3 (StatsOrderTime 9 0 Nothing) Nothing)
            , (Queue 2, SimFlowTimeStats 3 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, SimFlowTimeStats 2 (StatsOrderTime 7 0 Nothing) Nothing)
            , (Machine 2, SimFlowTimeStats 2 (StatsOrderTime 7 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         = M.fromList [(OrderPool, StatsProcTime 0), (Queue 1, StatsProcTime 0), (Queue 2, StatsProcTime 6), (Machine 1, StatsProcTime 7), (Machine 2, StatsProcTime 7)]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          SimFlowTimeStats
          { statsNrOrders = 1
          , statsOrderFlowTime = StatsOrderTime 6 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          SimFlowTimeStats
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0


--
-- Simulation1Spec.hs ends here
