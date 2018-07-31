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
--     Update #: 27
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

prettySimStatistics :: SimStatistics -> Doc
prettySimStatistics (SimStatistics bls blTimes sf sfFgi costs) =
  nest 2 $
  text "Statistics:" <$$>
  vcat (map (\(b,s) -> nest 2 (text (show b) <$$> prettyStats s)) (M.toList bls)) <$$>
  empty


prettyStats :: SimStats -> Doc
prettyStats (SimStats nr time tard) =
  text "orders seen:" <+> integer nr <$$>
  nest 2 (text "flow time:" <$$> prettyOrderTime nr time) <$$>
  nest 2 (text "tardiness:" <$$> prettyOrderTardiness nr tard)

prettyOrderTime :: Integer -> StatsOrderTime -> Doc
prettyOrderTime nr (StatsOrderTime sumTime stdDev) =
  text "mean: " <+> double (sumTime / fromInteger nr) <$$>
  text "standard deviation:" <+> double stdDev


prettyOrderTardiness :: Integer -> StatsOrderTard -> Doc
prettyOrderTardiness nr (StatsOrderTard nrTard sumTime stdDev) =
  text "tardiness in %:" <+> double (fromInteger nrTard / fromInteger nr) <$$>
  text "mean: " <+> double (sumTime / fromInteger nr) <$$>
  text "standard deviation:" <+> double stdDev


--
-- Pretty.hs ends here
