-- Block.hs ---
--
-- Filename: Block.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 16:22:10 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 5
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

module SimSim.Block where

import           ClassyPrelude

data Block = Source | Sink | FGI | Machine Int | Queue Int
  deriving (Show, Eq, Ord)


--
-- Block.hs ends here
