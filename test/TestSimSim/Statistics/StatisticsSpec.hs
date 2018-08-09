-- StatisticsSpec.hs ---
--
-- Filename: StatisticsSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:55:42 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 26
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


module TestSimSim.Statistics.StatisticsSpec (spec) where

import qualified Data.Map.Strict                 as M
import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.Order
import           SimSim.Simulation
import           SimSim.Statistics.Internal
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Type
import           SimSim.Time

import           TestSimSim.Block.Instances
import           TestSimSim.Order.Instances
import           TestSimSim.Simulation.Instances


spec :: Spec
spec = describe "Statistics Functions" $ do
  it "prop_blockFlowTime" $ property prop_blockFlowTime
  it "prop_updateCosts" $ property prop_updateCosts
  it "prop_updateTardiness" $ property prop_updateTardiness
  it "prop_statsAddRelease" $ property prop_statsAddRelease
  it "prop_statsAddEndProduction" $ property prop_statsAddEndProduction
  it "prop_statsAddShipped" $ property prop_statsAddShipped


prop_blockFlowTime :: Update -> Order -> Property
prop_blockFlowTime bl@Shipped o = isJust (shipped o) ==> fromTime (fromJust (shipped o) - fromJust (released o)) === getBlockFlowTime bl o
prop_blockFlowTime bl@EndProd o = isJust (prodEnd o) ==> fromTime (fromJust (prodEnd o) - fromJust (released o)) === getBlockFlowTime bl o
prop_blockFlowTime bl@(UpBlock OrderPool) o = isJust (released o) ==> fromTime (fromJust (released o) - arrivalDate o) === getBlockFlowTime bl o
prop_blockFlowTime bl@(UpBlock FGI) o = isJust (shipped o) ==> fromTime (fromJust (shipped o) - fromJust (prodEnd o)) === getBlockFlowTime bl o
prop_blockFlowTime bl@(UpBlock Sink) o = expectFailure $ property $ getBlockFlowTime bl o > 0
prop_blockFlowTime bl o = property $ fromTime (orderCurrentTime o - lastBlockStart o) === getBlockFlowTime bl o

prop_updateCosts :: Update -> Order -> StatsOrderCost -> Property
prop_updateCosts Shipped o st@(StatsOrderCost earn wip bo fgi) = property $ StatsOrderCost (earn+1) wip bo fgi == updateCosts Shipped o st
prop_updateCosts up o st = property $ st == updateCosts up o st


prop_updateTardiness :: Update -> Order -> StatsOrderTard -> Property
prop_updateTardiness EndProd o st@(StatsOrderTard nr sum stdDev)
  | maybe True (dueDate o >=) (prodEnd o) = property $ st == updateTardiness EndProd o st
  | otherwise = property $ StatsOrderTard (nr+1) (sum - fromTime (dueDate o) + fromTime (fromJust (prodEnd o))) stdDev == updateTardiness EndProd o st
prop_updateTardiness up o st = property $ st == updateTardiness up o st


prop_statsAddRelease :: Order -> SimSim -> Property
prop_statsAddRelease o sim = isJust (released o) && isNothing (prodStart o) ==> property $ sim {simStatistics = stats {simStatsBlock = block', simStatsBlockTimes = times'}} === statsAddRelease o sim
  where
    stats = simStatistics sim
    addStatsOrderTime (StatsOrderTime s1 stdDev1) (StatsOrderTime s2 stdDev2) = StatsOrderTime (s1 + s2) 0 -- TODO stdDev
    addStatsOrderTard (StatsOrderTard nr1 s1 stdDev1) (StatsOrderTard nr2 s2 stdDev2) = StatsOrderTard (nr1 + nr2) (s1 + s2) 0 -- TODO stdDev
    updateFunBlock vOld vNew =
      (vOld
       { statsNrOrders = statsNrOrders vOld + 1
       , statsOrderFlowTime = statsOrderFlowTime vOld `addStatsOrderTime` statsOrderFlowTime vNew
       , statsOrderTardiness = statsOrderTardiness vOld `addStatsOrderTard` statsOrderTardiness vNew
       })
    simStatsSingleton f o =
      SimStats
        1
        (StatsOrderTime (f o) 0)
        emptyStatsOrderTard     -- only reported when shipped
      where
        tard = maybe 0 (fromTime . max 0 . subtract (dueDate o)) (shipped o)
    singRel = simStatsSingleton (\o -> fromTime (fromJust (released o) - arrivalDate o)) o
    block' = M.insertWith updateFunBlock OrderPool singRel (simStatsBlock stats)
    addStatsOrderTimeRelease (StatsOrderTime sm stdDev) o = StatsOrderTime (sm + fromTime (fromJust (released o) - arrivalDate o)) stdDev
    updateFunBlockTime vOld vNew = vOld {statsProcessing = statsProcessing vOld + statsProcessing vNew}
    times' = M.insertWith updateFunBlockTime OrderPool (StatsBlockTime (fromTime (fromJust (released o) - arrivalDate o))) (simStatsBlockTimes stats)


prop_statsAddEndProduction :: Order -> SimSim -> Property
prop_statsAddEndProduction o sim = isJust (prodEnd o) && isNothing (shipped o) ==> False === error "not yet implemented"


prop_statsAddShipped :: Order -> SimSim -> Property
prop_statsAddShipped o sim = isJust (shipped o) ==> False === error "not yet implemented"


--
-- StatisticsSpec.hs ends here
