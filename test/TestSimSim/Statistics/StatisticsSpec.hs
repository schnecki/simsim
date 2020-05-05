{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
--     Update #: 124
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
import           Text.PrettyPrint.ANSI.Leijen

import           SimSim.Block
import           SimSim.Order
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Simulation
import           SimSim.Simulation.Type
import           SimSim.Statistics.Internal
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Pretty
import           SimSim.Statistics.Type
import           SimSim.Time

import           TestSimSim.Block.Instances
import           TestSimSim.Order.Instances
import           TestSimSim.Simulation.Instances
import           TestSimSim.Statistics.Instances
import           TestSimSim.Util                 hiding ((===))


spec :: Spec
spec = do
  describe "Statistics Internal Functions" $ do
    it "prop_blockFlowTime" $ property prop_blockFlowTime
    it "prop_updateCosts" $ property prop_updateCosts
    it "prop_updateTardiness" $ property prop_updateTardiness
  describe "Statistics Exports" $ do
    -- it "prop_statsAddRelease" $ property prop_statsAddRelease
    it "prop_statsAddEndProduction" $ property prop_statsAddEndProduction
    -- it "prop_statsAddShipped" $ property prop_statsAddShipped

instance Pretty Rational where
  pretty = text . show

prop_blockFlowTime :: Update -> Order -> Property
prop_blockFlowTime bl@Shipped o = isJust (shipped o) ==> fromTime (fromJust (shipped o) - fromJust (released o)) === getBlockFlowTime bl o
prop_blockFlowTime bl@EndProd o = isJust (prodEnd o) ==> fromTime (fromJust (prodEnd o) - fromJust (released o)) === getBlockFlowTime bl o
prop_blockFlowTime bl@(UpBlock OrderPool) o = isJust (released o) ==> fromTime (fromJust (released o) - arrivalDate o) === getBlockFlowTime bl o
prop_blockFlowTime bl@(UpBlock FGI) o = isJust (shipped o) ==> fromTime (fromJust (shipped o) - fromJust (prodEnd o)) === getBlockFlowTime bl o
prop_blockFlowTime bl@(UpBlock Sink) o = expectFailure $ property $ getBlockFlowTime bl o > 0
prop_blockFlowTime bl o = property $ fromTime (orderCurrentTime o - blockStartTime o) === getBlockFlowTime bl o

prop_updateCosts :: Update -> Order -> StatsOrderCost -> Property
prop_updateCosts Shipped o st@(StatsOrderCost earn wip bo fgi) = property $ StatsOrderCost (earn+1) wip bo fgi === updateCosts Shipped o st
prop_updateCosts up o st = property $ st === updateCosts up o st


prop_updateTardiness :: Order -> StatsOrderTard -> Property
prop_updateTardiness o st = forAll sizedUpdate (conjoin . map (\u -> prop_updateTardiness' u o st))
  where
  prop_updateTardiness' EndProd o st@(StatsOrderTard nr sum stdDev@(StatsStdDev count mean m2))
    | maybe True (dueDate o >=) (prodEnd o) = property $ st === updateTardiness EndProd o st
    | otherwise = property $ StatsOrderTard (nr+1) (sum - fromTime (dueDate o) + fromTime (fromJust (prodEnd o))) (StatsStdDev count' mean' m2') === updateTardiness EndProd o st
      where tard = fromMaybe (error "was not tardy in production") (orderTardinessProduction o)
            (count', mean', m2') = welford count mean m2 (fromTime tard)
  prop_updateTardiness' Shipped o st@(StatsOrderTard nr sum stdDev@(StatsStdDev count mean m2))
    | maybe True (dueDate o >=) (shipped o) = property $ st === updateTardiness Shipped o st
    | otherwise = property $ StatsOrderTard (nr+1) (sum - fromTime (dueDate o) + fromTime (fromJust (shipped o))) (StatsStdDev count' mean' m2') === updateTardiness Shipped o st
      where tard = fromMaybe (error "was not tardy in production") (orderTardinessShipped o)
            (count', mean', m2') = welford count mean m2 (fromTime tard)

  prop_updateTardiness' up o st = property $ st === updateTardiness up o st


welford count mean m2 value =
  let count' = count + 1
      delta = value - mean
      mean' = mean + delta / fromIntegral count'
      delta2 = value - mean'
      m2' = m2 + delta * delta2
  in (count', mean', m2')


addStatsStdDev :: StatsStdDev -> StatsStdDev -> StatsStdDev
addStatsStdDev (StatsStdDev counter1 mean1 m21) (StatsStdDev counter2 mean2 m22) =
  StatsStdDev (counter1 + counter1) (mean1 + mean2) (m21 + m22)

prop_statsAddRelease :: Order -> SimSim -> Property
prop_statsAddRelease o sim = isJust (released o) && isNothing (prodStart o) ==> property $ eqPretty (sim {simStatistics = stats {simStatsBlockFlowTimes = block', simStatsBlockProcTimes = times'}}) (prettySimStatistics True) (statsAddRelease o sim) (prettySimStatistics True)
  where
    stats = simStatistics sim
    addStatsOrderTime (StatsOrderTime s1 stdDev1 partial1) (StatsOrderTime s2 stdDev2 partial2) = StatsOrderTime (s1 + s2) (addStatsStdDev stdDev1 stdDev2)  Nothing -- TODO stdDev
    addStatsOrderTard (StatsOrderTard nr1 s1 stdDev1) (StatsOrderTard nr2 s2 stdDev2) = StatsOrderTard (nr1 + nr2) (s1 + s2) (addStatsStdDev stdDev1 stdDev2) -- TODO stdDev
    updateFunBlock vOld vNew =
      (vOld
       { statsNrOrders = statsNrOrders vOld + 1
       , statsOrderFlowTime = statsOrderFlowTime vOld `addStatsOrderTime` statsOrderFlowTime vNew
       , statsOrderTardiness = Nothing -- statsOrderTardiness vOld `addStatsOrderTard` statsOrderTardiness vNew
       })
    simStatsSingleton f o =
      StatsFlowTime
        1
        (StatsOrderTime (f o) (StatsStdDev 0 0 0) Nothing)
        Nothing
      where
        tard = maybe 0 (fromTime . max 0 . subtract (dueDate o)) (shipped o)
    singRel = simStatsSingleton (\o -> fromTime (fromJust (released o)) - fromTime (arrivalDate o)) o
    block' = M.insertWith updateFunBlock OrderPool singRel (simStatsBlockFlowTimes stats)
    addStatsOrderTimeRelease (StatsOrderTime sm stdDev partial) o = StatsOrderTime (sm + fromTime (fromJust (released o) - arrivalDate o)) stdDev -- TODO
    updateFunBlockTime vOld vNew = vOld {statsBlockTime = statsBlockTime vOld + statsBlockTime vNew}
    times' = M.insertWith updateFunBlockTime OrderPool (StatsProcTime (fromTime (fromJust (released o) - arrivalDate o))) (simStatsBlockProcTimes stats)


prop_statsAddEndProduction :: Order -> SimSim -> Property
prop_statsAddEndProduction o simIn =
  isJust (prodEnd o) &&
  isNothing (shipped o)
  -- productTypeNr (productType o) <= length (productTypes simIn)
  ==>
  -- trace ("routes: " ++ show (simRouting simIn)) $

  eqPretty (sim {simStatistics = stats {simStatsShopFloor = simStatsShopFloor'}}) (prettySimStatistics True) (statsAddEndProduction o sim) (prettySimStatistics True)
  where
    sim = simIn {simOrdersFgi = [o] }
    stats = simStatistics sim
    s = simStatsShopFloor stats
    simStatsShopFloor' =
      s
      { statsNrOrders = statsNrOrders s + 1
      , statsOrderFlowTime = addFT (statsOrderFlowTime s) o
      , statsOrderTardiness = Just $ addTard (fromMaybe emptyStatsOrderTard (statsOrderTardiness s)) o
      }
    addFT (StatsOrderTime sumT stdDev@(StatsStdDev count mean m2) partial) o = StatsOrderTime (sumT + t) (StatsStdDev count' mean' m2') Nothing
      where
        t = fromTime $ fromJust (prodEnd o) - fromJust (released o)
        (count', mean', m2') = welford count mean m2 t
    addTard s@(StatsOrderTard nr sumT stdDev@(StatsStdDev count mean m2)) o = case orderTardinessProduction o of
      Nothing -> s
      Just lateness -> StatsOrderTard (nr + 1) (sumT + fromTime lateness) (StatsStdDev count' mean' m2')
        where
          (count', mean', m2') = welford count mean m2 (fromTime lateness)

prop_statsAddShipped :: Order -> SimSim -> Property
prop_statsAddShipped o sim =
  isJust (shipped o) ==>
  sim
  {simStatistics = stats {simStatsBlockFlowTimes = blocks', simStatsBlockProcTimes = blockTimes', simStatsShopFloorAndFgi = simStatsShopFloorAndFgi', simStatsOrderCosts = costs'}} ===
  statsAddShipped [o] [o] sim
  where
    stats = simStatistics sim
    -- FGI
    sing = StatsFlowTime 1 (addFT emptyStatsOrderTime o) Nothing
      where
        addFT (StatsOrderTime sumT stdDev partial) o = StatsOrderTime (sumT + t) (StatsStdDev 0 0 0) Nothing -- TODO
        t = fromTime $ fromJust (shipped o) - fromJust (prodEnd o)
    singTimes = StatsProcTime (fromTime $ fromJust (shipped o) - fromJust (prodEnd o))
    addStatsFlowTime (StatsFlowTime nr1 ft1 tard1) (StatsFlowTime nr2 ft2 tard2) = StatsFlowTime (nr1 + nr2) (addFtStats ft1 ft2) Nothing
    addFtStats (StatsOrderTime s1 stdDev1 partial1) (StatsOrderTime s2 stdDev2 partial2) = StatsOrderTime (s1 + s2) (StatsStdDev 0 0 0) Nothing -- TODO
    addTardStats (StatsOrderTard nr1 s1 stdDev1) (StatsOrderTard nr2 s2 stdDev2) = StatsOrderTard (nr1 + nr2) (s1 + s2) (StatsStdDev 0 0 0) -- TODO
    blocks' = M.insertWith addStatsFlowTime FGI sing (simStatsBlockFlowTimes stats)
    addStatsFlowTimeProcTimes (StatsProcTime t1) (StatsProcTime t2) = StatsProcTime (t1 + t2)
    blockTimes' = M.insertWith addStatsFlowTimeProcTimes FGI singTimes (simStatsBlockProcTimes stats)
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
        addFT (StatsOrderTime sumT stdDev partial) o = StatsOrderTime (sumT + t) (StatsStdDev 0 0 0) Nothing -- TODO stdev
        t = fromTime $ fromJust (shipped o) - fromJust (released o)
    addTard s@(StatsOrderTard nr sumT stdDev) o
      | lateness <= 0 = s
      | otherwise = StatsOrderTard (nr + 1) (sumT + lateness) (StatsStdDev 0 0 0)
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
