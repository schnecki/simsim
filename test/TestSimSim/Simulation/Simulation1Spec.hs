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
--     Update #: 202
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

import           Control.Monad                       (guard, liftM)
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
    it "prop_simulation1 Time=8" $ prop_simulation1AtTime 8 simAtTime8
    it "prop_simulation1 Time=9" $ prop_simulation1AtTime 9 simAtTime9
    it "prop_simulation1 Time=10" $ prop_simulation1AtTime 10 simAtTime10
    it "prop_simulation1 Time=11" $ prop_simulation1AtTime 11 simAtTime11
    it "prop_simulation1 Time=12" $ prop_simulation1AtTime 12 simAtTime12
    it "prop_simulation1 Time=15" $ prop_simulation1AtTime 15 simAtTime15
    it "prop_simulation1 Time=20" $ prop_simulation1AtTime 20 simAtTime20
    it "prop_simulation1 Time=21" $ prop_simulation1AtTime 21 simAtTime21
    it "prop_simulation1 Time=30" $ prop_simulation1AtTime 30 simAtTime30
  describe "Simulation combinations" $ do
    it "prop_simulation1 1plus1Eq2" $ prop_simulation1plus1Eq2
    it "prop_simulation1 plus2Eq3" $ prop_simulation1plus2Eq3
    it "prop_simulation1 AddTwo" $ property prop_simulation1AddTwo


prop_simulation1AtTime :: Time -> (SimSim -> SimSim) -> Property
prop_simulation1AtTime t f =
  ioProperty $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    sim' <- simulateUntil t sim orders
    return $ eqPretty (f sim) (prettySimulation True prettyOrderDue) sim' (prettySimulation True prettyOrderDue)
    -- return $ f sim ==== sim'


propList :: [(Time -> (SimSim -> SimSim) -> Property, Time, SimSim -> SimSim)]
propList =
  [ (prop_simulation1AtTime, 1, simAtTime1)
  , (prop_simulation1AtTime, 2, simAtTime2)
  , (prop_simulation1AtTime, 3, simAtTime3)
  , (prop_simulation1AtTime, 4, simAtTime4)
  , (prop_simulation1AtTime, 5, simAtTime5)
  , (prop_simulation1AtTime, 6, simAtTime6)
  , (prop_simulation1AtTime, 7, simAtTime7)
  , (prop_simulation1AtTime, 8, simAtTime8)
  , (prop_simulation1AtTime, 9, simAtTime9)
  , (prop_simulation1AtTime, 10, simAtTime10)
  , (prop_simulation1AtTime, 11, simAtTime11)
  , (prop_simulation1AtTime, 12, simAtTime12)
  , (prop_simulation1AtTime, 15, simAtTime15)
  , (prop_simulation1AtTime, 20, simAtTime20)
  , (prop_simulation1AtTime, 21, simAtTime21)
  , (prop_simulation1AtTime, 30, simAtTime30)
  ]


prop_simulation1AddTwo :: Double -> Double -> Property
prop_simulation1AddTwo stop end =
  stop >= 0 && stop < end ==> ioProperty $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    sim1 <- simulateUntil (Time $ toRational stop) sim orders
    sim2 <- simulateUntil (Time $ toRational end) sim1 []
    sim' <- simulateUntil (Time $ toRational end) sim orders
    return $
      eqPretty
        sim'
        (prettySimulation True prettyOrderDue)
        sim2
        (\x -> prettySimulation True prettyOrderDue x <$$> text "Numbers: " <> double stop <+> text "-->" <+> double end <+> text " == " <+> double end)


prop_simulation1plus1Eq2 :: Property
prop_simulation1plus1Eq2 = ioProperty $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    sim1 <- simulateUntil 0.833 sim orders
    sim2 <- simulateUntil 11.0 sim1 []
    sim' <- simulateUntil 11.0 sim orders
    return $ eqPretty sim' (prettySimulation True prettyOrderDue) sim2 (prettySimulation True prettyOrderDue)


prop_simulation1plus2Eq3 :: Property
prop_simulation1plus2Eq3 = ioProperty $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    sim1 <- simulateUntil 1 sim orders
    sim2 <- simulateUntil 3 sim1 []
    sim' <- simulateUntil 3 sim orders
    return $ eqPretty sim' (prettySimulation True prettyOrderDue) sim2 (prettySimulation True prettyOrderDue)


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
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 0 (StatsOrderTime 1 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            , (Machine 2, StatsFlowTime 0 (StatsOrderTime 1 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            ]
      , simStatsBlockProcTimes =
          M.fromList
              -- idle times
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 1)
            , (FGI, StatsProcTime 1)
              -- proc times
            , (Machine 1, StatsProcTime 1)
            , (Machine 2, StatsProcTime 1)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          StatsFlowTime
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
  , simStatistics = statsAtTime2
  }
  where
    statsAtTime2 :: SimStatistics
    statsAtTime2 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 0 (StatsOrderTime 2 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            , (Machine 2, StatsFlowTime 0 (StatsOrderTime 2 0 (Just $ StatsOrderTime 0 0 Nothing)) Nothing)
            ]
      , simStatsBlockProcTimes =
          M.fromList
              -- idle times
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 2)
            , (FGI, StatsProcTime 2)
              -- proc times
            , (Machine 1, StatsProcTime 2)
            , (Machine 2, StatsProcTime 2)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          StatsFlowTime
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
  , simStatistics = statsAtTime3
  }
  where
    statsAtTime3 :: SimStatistics
    statsAtTime3 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 2 (StatsOrderTime 3 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 1 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 1 (StatsOrderTime 3 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 0 (StatsOrderTime 3 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 3)
            , (FGI, StatsProcTime 3)
            , (Machine 1, StatsProcTime 3)
            , (Machine 2, StatsProcTime 3)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          StatsFlowTime
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
  , simStatistics = statsAtTime4
  }
  where
    statsAtTime4 :: SimStatistics
    statsAtTime4 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 2 (StatsOrderTime 3 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 2 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 1 (StatsOrderTime 4 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 1 (StatsOrderTime 4 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- processing times
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 3)
            , (FGI, StatsProcTime 4)
            , (Machine 1, StatsProcTime 4)
            , (Machine 2, StatsProcTime 4)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          StatsFlowTime
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
  , simStatistics = statsAtTime5
  }
  where
    statsAtTime5 :: SimStatistics
    statsAtTime5 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 2 (StatsOrderTime 3 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 2 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 1 (StatsOrderTime 5 0 (Just $ StatsOrderTime 3 0 Nothing)) Nothing)
            , (Machine 2, StatsFlowTime 1 (StatsOrderTime 5 0 (Just $ StatsOrderTime 3 0 Nothing)) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 4)
            , (FGI, StatsProcTime 5)
            , (Machine 1, StatsProcTime 5)
            , (Machine 2, StatsProcTime 5)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsShopFloorAndFgi =
          StatsFlowTime
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
  , simStatistics = statsAtTime6
  }
  where
    statsAtTime6 :: SimStatistics
    statsAtTime6 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of  fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 3 (StatsOrderTime 9 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 3 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 2 (StatsOrderTime 6 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 2 (StatsOrderTime 6 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 5)
            , (FGI, StatsProcTime 6)
            , (Machine 1, StatsProcTime 6)
            , (Machine 2, StatsProcTime 6)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 1, statsOrderFlowTime = StatsOrderTime 6 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsShopFloorAndFgi =
          StatsFlowTime
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
  , simStatistics = statsAtTime7
  }
  where
    statsAtTime7 :: SimStatistics
    statsAtTime7 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 3 (StatsOrderTime 9 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 3 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 2 (StatsOrderTime 7 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 2 (StatsOrderTime 7 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 6)
            , (FGI, StatsProcTime 6)
            , (Machine 1, StatsProcTime 7)
            , (Machine 2, StatsProcTime 7)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 1, statsOrderFlowTime = StatsOrderTime 6 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0


simAtTime8 :: SimSim -> SimSim
simAtTime8 sim =
  sim
  { simCurrentTime = 8
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, [orders !! 1]), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 3, 1))]
  , simOrdersFgi = [orders !! 0, orders !! 2]
  , simOrdersShipped = []
  , simStatistics = statsAtTime8
  }
  where
    statsAtTime8 :: SimStatistics
    statsAtTime8 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList            -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 3 (StatsOrderTime 9 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 3 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 2 (StatsOrderTime 8 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 3 (StatsOrderTime 8 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 7)
            , (FGI, StatsProcTime 6)
            , (Machine 1, StatsProcTime 8)
            , (Machine 2, StatsProcTime 8)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 2, statsOrderFlowTime = StatsOrderTime 14 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0


simAtTime9 :: SimSim -> SimSim
simAtTime9 sim =
  sim
  { simCurrentTime = 9
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 1, 2)), (Machine 2, (orders !! 3, 2))]
  , simOrdersFgi = [orders !! 0, orders !! 2]
  , simOrdersShipped = []
  , simStatistics = statsAtTime9
  }
  where
    statsAtTime9 :: SimStatistics
    statsAtTime9 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 3 (StatsOrderTime 9 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 3 (StatsOrderTime 8 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 0)
            , (Queue 2, StatsProcTime 8)
            , (FGI, StatsProcTime 6)
            , (Machine 1, StatsProcTime 9)
            , (Machine 2, StatsProcTime 8)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 2, statsOrderFlowTime = StatsOrderTime 14 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 0
          , statsOrderFlowTime = StatsOrderTime 0 0 Nothing
          , statsOrderTardiness = Nothing -- Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 0 0 0 0


simAtTime10 :: SimSim -> SimSim
simAtTime10 sim =
  sim
  { simCurrentTime = 10
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList [(Machine 1, (orders !! 1, 1)), (Machine 2, (orders !! 3, 1))]
  , simOrdersFgi = []
  , simOrdersShipped = [orders !! 0, orders !! 2]
  , simStatistics = statsAtTime10
  }
  where
    statsAtTime10 :: SimStatistics
    statsAtTime10 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 3 (StatsOrderTime 10 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 3 (StatsOrderTime 9 0 Nothing) Nothing)
            , (FGI, StatsFlowTime 2 (StatsOrderTime 6 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 1)
            , (Queue 2, StatsProcTime 9)
            , (FGI, StatsProcTime 6)
            , (Machine 1, StatsProcTime 10)
            , (Machine 2, StatsProcTime 9)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 2, statsOrderFlowTime = StatsOrderTime 14 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 2
          , statsOrderFlowTime = StatsOrderTime 20 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 2 2 2 0


simAtTime11 :: SimSim -> SimSim
simAtTime11 sim =
  sim
  { simCurrentTime = 11
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList []
  , simOrdersFgi = [orders !! 1, orders !! 3]
  , simOrdersShipped = [orders !! 0, orders !! 2]
  , simStatistics = statsAtTime11
  }
  where
    statsAtTime11 :: SimStatistics
    statsAtTime11 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 4 (StatsOrderTime 11 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 4 (StatsOrderTime 10 0 Nothing) Nothing)
            , (FGI, StatsFlowTime 2 (StatsOrderTime 6 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 2)
            , (Queue 2, StatsProcTime 10)
            , (FGI, StatsProcTime 7)
            , (Machine 1, StatsProcTime 11)
            , (Machine 2, StatsProcTime 10)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 4, statsOrderFlowTime = StatsOrderTime 36 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 2 2 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 2
          , statsOrderFlowTime = StatsOrderTime 20 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 2 2 2 0

simAtTime12 :: SimSim -> SimSim
simAtTime12 sim =
  sim
  { simCurrentTime = 12
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList []
  , simOrdersFgi = [orders !! 1, orders !! 3]
  , simOrdersShipped = [orders !! 0, orders !! 2]
  , simStatistics = statsAtTime12
  }
  where
    statsAtTime12 :: SimStatistics
    statsAtTime12 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 4 (StatsOrderTime 11 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 4 (StatsOrderTime 10 0 Nothing) Nothing)
            , (FGI, StatsFlowTime 2 (StatsOrderTime 6 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 3)
            , (Queue 2, StatsProcTime 11)
            , (FGI, StatsProcTime 7)
            , (Machine 1, StatsProcTime 11)
            , (Machine 2, StatsProcTime 10)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 4, statsOrderFlowTime = StatsOrderTime 36 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 2 2 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 2
          , statsOrderFlowTime = StatsOrderTime 20 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 2 2 2 0


simAtTime15 :: SimSim -> SimSim
simAtTime15 sim =
  sim
  { simCurrentTime = 15
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList []
  , simOrdersFgi = [orders !! 1, orders !! 3]
  , simOrdersShipped = [orders !! 0, orders !! 2]
  , simStatistics = statsAtTime15
  }
  where
    statsAtTime15 :: SimStatistics
    statsAtTime15 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 4 (StatsOrderTime 11 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 4 (StatsOrderTime 10 0 Nothing) Nothing)
            , (FGI, StatsFlowTime 2 (StatsOrderTime 6 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 6)
            , (Queue 2, StatsProcTime 14)
            , (FGI, StatsProcTime 7)
            , (Machine 1, StatsProcTime 11)
            , (Machine 2, StatsProcTime 10)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 4, statsOrderFlowTime = StatsOrderTime 36 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 2 2 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 2
          , statsOrderFlowTime = StatsOrderTime 20 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 0 0 0
          }
        statsOrderCost = StatsOrderCost 2 2 2 0


simAtTime20 :: SimSim -> SimSim
simAtTime20 sim =
  sim
  { simCurrentTime = 20
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList []
  , simOrdersFgi = []
  , simOrdersShipped = [orders !! 1, orders !! 3]
  , simStatistics = statsAtTime20
  }
  where
    statsAtTime20 :: SimStatistics
    statsAtTime20 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 4 (StatsOrderTime 11 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 4 (StatsOrderTime 10 0 Nothing) Nothing)
            , (FGI, StatsFlowTime 4 (StatsOrderTime 24 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 11)
            , (Queue 2, StatsProcTime 19)
            , (FGI, StatsProcTime 7)
            , (Machine 1, StatsProcTime 11)
            , (Machine 2, StatsProcTime 10)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 4, statsOrderFlowTime = StatsOrderTime 36 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 2 2 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 4
          , statsOrderFlowTime = StatsOrderTime 60 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 2 20 0
          }
        statsOrderCost = StatsOrderCost 4 2 2 0

simAtTime21 :: SimSim -> SimSim
simAtTime21 sim =
  sim
  { simCurrentTime = 21
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList []
  , simOrdersFgi = []
  , simOrdersShipped = [orders !! 1, orders !! 3]
  , simStatistics = statsAtTime21
  }
  where
    statsAtTime21 :: SimStatistics
    statsAtTime21 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 4 (StatsOrderTime 11 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 4 (StatsOrderTime 10 0 Nothing) Nothing)
            , (FGI, StatsFlowTime 4 (StatsOrderTime 24 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 12)
            , (Queue 2, StatsProcTime 20)
            , (FGI, StatsProcTime 8)
            , (Machine 1, StatsProcTime 11)
            , (Machine 2, StatsProcTime 10)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 4, statsOrderFlowTime = StatsOrderTime 36 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 2 2 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 4
          , statsOrderFlowTime = StatsOrderTime 60 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 2 20 0
          }
        statsOrderCost = StatsOrderCost 4 2 2 0


simAtTime30 :: SimSim -> SimSim
simAtTime30 sim =
  sim
  { simCurrentTime = 30
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList [(Machine 1, []), (Machine 2, [])]
  , simOrdersMachine = M.fromList []
  , simOrdersFgi = []
  , simOrdersShipped = []
  , simStatistics = statsAtTime30
  }
  where
    statsAtTime30 :: SimStatistics
    statsAtTime30 =
      SimStatistics
      { simStatsBlockFlowTimes =
          M.fromList -- flow time of fully finished queue orders only, but full machine processing times
            [ (OrderPool, StatsFlowTime 4 (StatsOrderTime 0 0 Nothing) Nothing)
            , (Queue 1, StatsFlowTime 4 (StatsOrderTime 14 0 Nothing) Nothing)
            , (Queue 2, StatsFlowTime 4 (StatsOrderTime 1 0 Nothing) Nothing)
            , (Machine 1, StatsFlowTime 4 (StatsOrderTime 11 0 Nothing) Nothing)
            , (Machine 2, StatsFlowTime 4 (StatsOrderTime 10 0 Nothing) Nothing)
            , (FGI, StatsFlowTime 4 (StatsOrderTime 24 0 Nothing) Nothing)
            ]
      , simStatsBlockProcTimes -- idle times for queues, processing times for machines and orderpool
         =
          M.fromList
            [ (OrderPool, StatsProcTime 0)
            , (Queue 1, StatsProcTime 21)
            , (Queue 2, StatsProcTime 29)
            , (FGI, StatsProcTime 17)
            , (Machine 1, StatsProcTime 11)
            , (Machine 2, StatsProcTime 10)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = StatsFlowTime {statsNrOrders = 4, statsOrderFlowTime = StatsOrderTime 36 0 Nothing, statsOrderTardiness = Just $ StatsOrderTard 2 2 0}
        statsShopFloorAndFgi =
          StatsFlowTime
          { statsNrOrders = 4
          , statsOrderFlowTime = StatsOrderTime 60 0 Nothing
          , statsOrderTardiness = Just $ StatsOrderTard 2 20 0
          }
        statsOrderCost = StatsOrderCost 4 2 2 0


--
-- Simulation1Spec.hs ends here
