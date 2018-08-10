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
--     Update #: 56
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

import           SimSim.Statistics.Type
import           SimSim.Time


time :: Rational -> Doc
time = double . fromRational


prettySimStatistics :: Time -> SimStatistics -> Doc
prettySimStatistics curTime (SimStatistics bls blTimes sf sfFgi costs) =
  nest 2 $
  text "Statistics:" <$$>
  nest 2 (text "Orders" <$$> vcat (map (\(b, s) -> nest 2 (text (show b) <$$> prettyStats s)) (M.toList bls))) <$$>
  nest 2 (text "Times" <$$> vcat (map (\(b, s) -> nest 2 (text (show b) <$$> prettyBlockTime curTime s)) (M.toList blTimes))) <$$>
  nest 2 (text "Shop Floor" <$$> prettyStats sf) <$$>
  nest 2 (text "Shop Floor and FGI" <$$> prettyStats sfFgi) <$$>
  nest 2 (text "Costs" <$$> prettyOrderCosts costs)


prettyStats :: SimStats -> Doc
prettyStats (SimStats nr time mTard) =
  text "orders seen:" <+> integer nr <$$> nest 2 (text "flow time:" <$$> prettyOrderTime nr time) <$$> maybe empty (\tard -> nest 2 (text "tardiness:" <$$> prettyOrderTardiness nr tard)) mTard

prettyOrderTime :: Integer -> StatsOrderTime -> Doc
prettyOrderTime nr (StatsOrderTime sumTime stdDev) =
  text "mean: " <+>
  time
    (if nr == 0
       then 0
       else sumTime / fromInteger nr) <$$>
  text "standard deviation:" <+>
  time stdDev


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


prettyBlockTime :: Time -> StatsBlockTime -> Doc
prettyBlockTime curTime (StatsBlockTime pr) = text "Processing in %:" <+> time (100 * pr / t) <$$> text "Idle in %:" <+> time (100 * (t - pr) / t)
  where
    t
      | curTime == 0 = 1
      | otherwise = fromTime curTime


prettyOrderCosts :: StatsOrderCost -> Doc
prettyOrderCosts (StatsOrderCost ear wip bo fgi) =
  text "Earnings (nr of occurrences):" <+> integer ear <$$> text "WIP costs:" <+> integer wip <$$> text "Back order costs:" <+> integer bo <$$> text "FGI Holding costs:" <+> integer fgi


--
-- Pretty.hs ends here
