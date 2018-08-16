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
--     Update #: 42
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
import           SimSim.Order.Type
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
spec = describe "Simulation runs" $ do
  it "prop_simulation1 Time=1" $ prop_simulation1AtTime 1 simAtTime1
  -- it "prop_simulation1 Time=10" $ prop_simulation1AtTime 10 simAtTime10


prop_simulation1AtTime :: Time -> (SimSim -> SimSim) -> Property
prop_simulation1AtTime t f =
  ioProperty $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    sim' <- simulateUntil t sim orders
    return $ f sim === sim'

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
  , simStatistics = statsAtTime10
  }
  where
    statsAtTime10 :: SimStatistics
    statsAtTime10 =
      SimStatistics
      { simStatsBlock = M.fromList []
      , simStatsBlockTimes = M.fromList []
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = SimStats {statsNrOrders = 0, statsOrderFlowTime = StatsOrderTime 0 0, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsShopFloorAndFgi = SimStats {statsNrOrders = 0, statsOrderFlowTime = StatsOrderTime 0 0, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsOrderCost = StatsOrderCost 0 0 0 0


simAtTime10 :: SimSim -> SimSim
simAtTime10 sim =
  sim
  { simCurrentTime = 10
  , simNextOrderId = 5
  , simOrdersOrderPool = []
  , simOrdersQueue = M.fromList []
  , simOrdersMachine = M.fromList []
  , simOrdersFgi = []
  , simOrdersShipped = []
  , simStatistics = statsAtTime10
  }
  where
    statsAtTime10 :: SimStatistics
    statsAtTime10 =
      SimStatistics
      { simStatsBlock =
          M.fromList
            [ (OrderPool, SimStats 4 (StatsOrderTime 0 0) Nothing)
            , (Queue 1,   SimStats 3 (StatsOrderTime 2 0) Nothing)
            , (Queue 2,   SimStats 1 (StatsOrderTime 0 0) Nothing)
            , (Machine 1, SimStats 1 (StatsOrderTime 1 0) Nothing)
            , (Machine 2, SimStats 1 (StatsOrderTime 1 0) Nothing)
            ]
      , simStatsBlockTimes =
          M.fromList
            [ (OrderPool, StatsBlockTime 0)
            , (Queue 1,   StatsBlockTime 1)
            , (Queue 2,   StatsBlockTime 0)
            , (Machine 1, StatsBlockTime 1)
            , (Machine 2, StatsBlockTime 1)
            ]
      , simStatsShopFloor = statsShopFloor
      , simStatsShopFloorAndFgi = statsShopFloorAndFgi
      , simStatsOrderCosts = statsOrderCost
      }
      where
        statsShopFloor = SimStats {statsNrOrders = 0, statsOrderFlowTime = StatsOrderTime 0 0, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsShopFloorAndFgi = SimStats {statsNrOrders = 0, statsOrderFlowTime = StatsOrderTime 0 0, statsOrderTardiness = Just $ StatsOrderTard 0 0 0}
        statsOrderCost = StatsOrderCost 0 0 0 0

--
-- Simulation1Spec.hs ends here
