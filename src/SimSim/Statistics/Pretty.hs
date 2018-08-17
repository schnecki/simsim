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
--     Update #: 86
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
prettySimStatistics updateStats sim =
  prettySimStatisticsInternal
    curTime
    (simStatistics sim)
    (simStatistics $
     if updateStats
       then simStatsUpdate
       else sim)
  where
    curTime = simCurrentTime sim
    blockOrderList =
      concatMap ((\os -> map (\o -> (lastBlock o, o)) os) . snd) (M.toList (simOrdersQueue sim)) ++
      map (second fst) (M.toList (simOrdersMachine sim)) ++ map (\x -> (FGI, x)) (simOrdersFgi sim)
    simStatsUpdate = foldl' (\s (b, o) -> statsAddBlock b (o {orderCurrentTime = curTime}) s) sim blockOrderList


prettySimStatisticsInternal :: Time -> SimStatistics -> SimStatistics -> Doc
prettySimStatisticsInternal curTime (SimStatistics bls _ sf sfFgi costs) (SimStatistics _ blTimes _ _ _) =
  nest 2 $
  text "Statistics:" <$$> nest 2 (text "Orders" <$$> vcat (map (\(b, s) -> nest 2 (text (show b) <$$> prettyStats s)) (M.toList bls))) <$$>
  nest 2 (text "Times" <$$> vcat (map (\(b, s) -> nest 2 (text (show b) <$$> prettyBlockTime b curTime s)) (M.toList blTimes))) <$$>
  nest 2 (text "Shop Floor" <$$> prettyStats sf) <$$>
  nest 2 (text "Shop Floor and FGI" <$$> prettyStats sfFgi) <$$>
  nest 2 (text "Costs" <$$> prettyOrderCosts costs)

prettyStats :: SimStats -> Doc
prettyStats (SimStats nr time mTard) =
  (\x -> maybe x (\tard -> x <$$> nest 2 (text "tardiness:" <$$> prettyOrderTardiness nr tard)) mTard) $
  text "orders seen:" <+> integer nr <$$> nest 2 (text "flow time:" <$$> prettyOrderTime nr time)


prettyOrderTime :: Integer -> StatsOrderTime -> Doc
prettyOrderTime nr stats@(StatsOrderTime _ _ partial) =
  text "mean: " <+>
  time
    (if nr == 0
       then 0
       else sumTime / fromInteger nr) <$$>
  text "standard deviation:" <+>
  time stdDev

  where sumTime = maybe (statsSumTime stats) statsSumTime partial
        stdDev = maybe (statsStdDevTime stats) statsStdDevTime partial


prettyOrderTardiness :: Integer -> StatsOrderTard -> Doc
prettyOrderTardiness nr (StatsOrderTard nrTard sumTime stdDev) =
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
  time stdDev


prettyBlockTime :: Block -> Time -> StatsBlockTime -> Doc
prettyBlockTime bl curTime (StatsBlockTime pTime) = text "Processing in %:" <+> time (100 * procTime / t) <$$> text "Idle in %:" <+> time (100 * (t - procTime) / t)
  where
    isIdleTime = isQueue bl
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
