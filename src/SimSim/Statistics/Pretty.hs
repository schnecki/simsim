-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Jul 31 19:08:22 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 106
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

module SimSim.Statistics.Pretty where

import           ClassyPrelude                hiding (empty, (<>))
import qualified Data.Map.Strict              as M
import           Text.PrettyPrint.ANSI.Leijen

import           SimSim.Block
import           SimSim.Order.Type
import           SimSim.Simulation.Type
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Type
import           SimSim.Time


time :: Rational -> Doc
time = double . fromRational

prettySimStatistics :: Bool -> SimSim -> Doc
prettySimStatistics isTest sim = prettySimStatisticsInternal isTest curTime (simStatistics sim)
  where
    curTime = simCurrentTime sim


prettySimStatisticsInternal :: Bool -> Time -> SimStatistics -> Doc
prettySimStatisticsInternal isTest curTime (SimStatistics bls blTimes sf sfFgi costs) =
  nest 2 $
  text "Statistics:" <$$>
  nest 2 (text "Orders Flow Times" <$$> vcat (map (\(b, s) -> nest 2 (text (show b) <$$> prettyStats isTest s)) (M.toList bls))) <$$>
  nest 2 (text "Processing Times" <$$> vcat (map (\(b, s) -> nest 2 (text (show b) <$$> prettyBlockTime isTest b curTime s)) (M.toList blTimes))) <$$>
  nest 2 (text "Shop Floor" <$$> prettyStats isTest sf) <$$>
  nest 2 (text "Shop Floor and FGI" <$$> prettyStats isTest sfFgi) <$$>
  nest 2 (text "Costs" <$$> prettyOrderCosts costs)

prettyStats :: Bool -> StatsFlowTime -> Doc
prettyStats isTest (StatsFlowTime nr time mTard) =
  (\x -> maybe x (\tard -> x <$$> nest 2 (text "tardiness:" <$$> prettyOrderTardiness isTest nr tard)) mTard) $
  text "orders seen:" <+> integer nr <$$> nest 2 (text "flow time:" <$$> prettyOrderTime isTest nr time)


prettyOrderTime :: Bool -> Integer -> StatsOrderTime -> Doc
prettyOrderTime False nr stats@(StatsOrderTime _ _ partial) =
  text "mean: " <+>
  time
    (if nr == 0
       then 0
       else sumTime / fromInteger nr) <+>
  text "standard deviation:" <+> time stdDev
  where
    sumTime = maybe (statsSumTime stats) statsSumTime partial
    stdDev = maybe (statsStdDevTime stats) statsStdDevTime partial
prettyOrderTime True nr stats@(StatsOrderTime sumTime stdDev _) =
  parens (time sumTime <+> char '/' <+> integer nr <> ";" <+> text (show $ stats {statsLastUpdatePartial = Nothing})) <$$> text "standard deviation:" <+> time stdDev


prettyOrderTardiness :: Bool -> Integer -> StatsOrderTard -> Doc
prettyOrderTardiness isTest nr stats@(StatsOrderTard nrTard sumTime stdDev) =
  text "tardiness in %:" <+>
  time
    (if nr == 0
       then 0
       else 100 * fromInteger nrTard / fromInteger nr) <$$>
  text "mean: " <+>
  time
    (if nr == 0
       then 0
       else sumTime / fromInteger nr) <$$>
  text "standard deviation:" <+>
  time stdDev <+>
  if isTest
    then text $ show stats
    else empty


prettyBlockTime :: Bool -> Block -> Time -> StatsProcTime -> Doc
prettyBlockTime isTest bl curTime (StatsProcTime pTime) =
  text "Processing in %:" <+>
  time (100 * procTime / t) <$$> text "Idle in %:" <+>
  time (100 * (t - procTime) / t) <+>
  if isTest
    then parens (time pTime)
    else empty
  where
    isIdleTime = not $ isMachine bl
    procTime :: Rational
    procTime
      | isIdleTime = fromTime curTime - pTime
      | otherwise = pTime
    t
      | curTime == 0 = 1
      | otherwise = fromTime curTime


prettyOrderCosts :: StatsOrderCost -> Doc
prettyOrderCosts (StatsOrderCost ear wip bo fgi) =
  text "Earnings (nr of occurrences):" <+> integer ear <$$> text "WIP costs:" <+> integer wip <$$> text "Back order costs:" <+> integer bo <$$> text "FGI Holding costs:" <+> integer fgi


--
-- Pretty.hs ends here
