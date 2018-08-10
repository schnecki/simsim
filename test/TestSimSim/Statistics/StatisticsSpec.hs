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
--     Update #: 70
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
import           Debug.Trace
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
import           TestSimSim.Statistics.Instances


spec :: Spec
spec = do
  describe "Statistics Internal Functions" $ do
    it "prop_blockFlowTime" $ property prop_blockFlowTime
    it "prop_updateCosts" $ property prop_updateCosts
    it "prop_updateTardiness" $ property prop_updateTardiness
  describe "Statistics Exports" $ do
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
prop_updateCosts Shipped o st@(StatsOrderCost earn wip bo fgi) = property $ StatsOrderCost (earn+1) wip bo fgi === updateCosts Shipped o st
prop_updateCosts up o st = property $ st === updateCosts up o st


prop_updateTardiness :: Order -> StatsOrderTard -> Property
prop_updateTardiness o st = forAll sizedUpdate (\us -> conjoin $ map (\u -> prop_updateTardiness' u o st) us)
  where
  prop_updateTardiness' EndProd o st@(StatsOrderTard nr sum stdDev)
    | maybe True (dueDate o >=) (prodEnd o) = property $ st === updateTardiness EndProd o st
    | otherwise = property $ StatsOrderTard (nr+1) (sum - fromTime (dueDate o) + fromTime (fromJust (prodEnd o))) stdDev === updateTardiness EndProd o st
  prop_updateTardiness' Shipped o st@(StatsOrderTard nr sum stdDev)
    | maybe True (dueDate o >=) (shipped o) = property $ st === updateTardiness Shipped o st
    | otherwise = property $ StatsOrderTard (nr+1) (sum - fromTime (dueDate o) + fromTime (fromJust (shipped o))) stdDev === updateTardiness Shipped o st
  prop_updateTardiness' up o st = property $ st === updateTardiness up o st


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
       , statsOrderTardiness = Nothing -- statsOrderTardiness vOld `addStatsOrderTard` statsOrderTardiness vNew
       })
    simStatsSingleton f o =
      SimStats
        1
        (StatsOrderTime (f o) 0)
        Nothing -- emptyStatsOrderTard     -- only reported when shipped
      where
        tard = maybe 0 (fromTime . max 0 . subtract (dueDate o)) (shipped o)
    singRel = simStatsSingleton (\o -> fromTime (fromJust (released o)) - fromTime (arrivalDate o)) o
    block' = M.insertWith updateFunBlock OrderPool singRel (simStatsBlock stats)
    addStatsOrderTimeRelease (StatsOrderTime sm stdDev) o = StatsOrderTime (sm + fromTime (fromJust (released o) - arrivalDate o)) stdDev -- TODO
    updateFunBlockTime vOld vNew = vOld {statsProcessing = statsProcessing vOld + statsProcessing vNew}
    times' = M.insertWith updateFunBlockTime OrderPool (StatsBlockTime (fromTime (fromJust (released o) - arrivalDate o))) (simStatsBlockTimes stats)


prop_statsAddEndProduction :: Order -> SimSim -> Property
prop_statsAddEndProduction o sim =
  isJust (prodEnd o) && isNothing (shipped o) ==> sim {simStatistics = stats {simStatsShopFloor = simStatsShopFloor'}} === statsAddEndProduction o sim
  where
    stats = simStatistics sim
    s = simStatsShopFloor stats
    simStatsShopFloor' =
      s
      { statsNrOrders = statsNrOrders s + 1
      , statsOrderFlowTime = addFT (statsOrderFlowTime s) o
      , statsOrderTardiness = Just $ addTard (fromMaybe emptyStatsOrderTard (statsOrderTardiness s)) o
      }
    addFT (StatsOrderTime sumT stdDev) o = StatsOrderTime (sumT + t) 0 -- TODO
      where
        t = fromTime $ fromJust (prodEnd o) - fromJust (released o)
    addTard s@(StatsOrderTard nr sumT stdDev) o
      | lateness <= 0 = s
      | otherwise = StatsOrderTard (nr + 1) (sumT + lateness) 0 -- TODO
      where
        lateness = max 0 (fromTime $ fromJust (prodEnd o) - dueDate o)

prop_statsAddShipped :: Order -> SimSim -> Property
prop_statsAddShipped o sim =
  isJust (shipped o) ==>
  sim {simStatistics = stats {simStatsBlock = blocks', simStatsBlockTimes = blockTimes', simStatsShopFloorAndFgi = simStatsShopFloorAndFgi', simStatsOrderCosts = costs'}} ===
  statsAddShipped o sim
  where
    stats = simStatistics sim
    -- FGI
    sing = SimStats 1 (addFT emptyStatsOrderTime o) Nothing
      where
        addFT (StatsOrderTime sumT stdDev) o = StatsOrderTime (sumT + t) 0 -- TODO
        t = fromTime $ fromJust (shipped o) - fromJust (prodEnd o)
    singTimes = StatsBlockTime (fromTime $ fromJust (shipped o) - fromJust (prodEnd o))
    addSimStats (SimStats nr1 ft1 tard1) (SimStats nr2 ft2 tard2) = SimStats (nr1 + nr2) (addFtStats ft1 ft2) Nothing
    addFtStats (StatsOrderTime s1 stdDev1) (StatsOrderTime s2 stdDev2) = StatsOrderTime (s1 + s2) 0 -- TODO
    addTardStats (StatsOrderTard nr1 s1 stdDev1) (StatsOrderTard nr2 s2 stdDev2) = StatsOrderTard (nr1 + nr2) (s1 + s2) 0 -- TODO
    blocks' = M.insertWith addSimStats FGI sing (simStatsBlock stats)
    addSimStatsBlockTimes (StatsBlockTime t1) (StatsBlockTime t2) = StatsBlockTime (t1 + t2)
    blockTimes' = M.insertWith addSimStatsBlockTimes FGI singTimes (simStatsBlockTimes stats)
    -- Costs
    costs = simStatsOrderCosts stats
    costs' = costs {statsEarnings = statsEarnings costs + 1}
    -- Shop floor
    s = simStatsShopFloorAndFgi stats
    simStatsShopFloorAndFgi' =
      s
      { statsNrOrders = statsNrOrders s + 1
      , statsOrderFlowTime = addFT (statsOrderFlowTime s) o
      , statsOrderTardiness = Just $ addTard (fromMaybe emptyStatsOrderTard (statsOrderTardiness s)) o
      }
      where
        addFT (StatsOrderTime sumT stdDev) o = StatsOrderTime (sumT + t) 0 -- TODO
        t = fromTime $ fromJust (shipped o) - fromJust (released o)
    addTard s@(StatsOrderTard nr sumT stdDev) o
      | lateness <= 0 = s
      | otherwise = StatsOrderTard (nr + 1) (sumT + lateness) 0 -- TODO
      where
        lateness = max 0 (fromTime $ fromJust (shipped o) - dueDate o)

prop_statsEndPeriodAddCosts :: SimSim -> Property
prop_statsEndPeriodAddCosts sim = sim { simStatistics = stats { simStatsOrderCosts = addCosts (simStatsOrderCosts stats) costs' }} === statsEndPeriodAddCosts sim
  where stats = simStatistics sim
        addCosts (StatsOrderCost e1 w1 b1 f1) (StatsOrderCost e2 w2 b2 f2) = StatsOrderCost (e1+e2) (w1+w2) (b1+b2) (f1+f2)
        opOrds = simOrdersOrderPool sim
        quOrds = concat (M.elems $ simOrdersQueue sim)
        maOrds = (M.elems $ fmap fst (simOrdersMachine sim))
        fgOrds = simOrdersFgi sim
        wipOrds = quOrds ++ maOrds
        boOrds = opOrds ++ wipOrds ++ fgOrds
        costs' = StatsOrderCost 0 (fromIntegral $ length wipOrds) (fromIntegral $ length boOrds) (fromIntegral $ length fgOrds)

--
-- StatisticsSpec.hs ends here
