-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  1 14:36:25 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 6
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

module SimSim.Time.Pretty where

import           ClassyPrelude                hiding (empty, (<>))
import qualified Data.Map.Strict              as M
import           Text.PrettyPrint.ANSI.Leijen

import           SimSim.Time.Type

instance Pretty Time where
  pretty = prettyTime

prettyTime :: Time -> Doc
prettyTime (Time t) = double (fromRational t)


--
-- Pretty.hs ends here
