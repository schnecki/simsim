-- Routing.hs ---
--
-- Filename: Routing.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 16:21:08 2017 (+0100)
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

module SimSim.Routing where

import           ClassyPrelude
import qualified Data.List.NonEmpty as NL

import           SimSim.Block
import           SimSim.Order
import           SimSim.ProductType

a --> b = (a,b)
infixl 2 -->

type Routing = NL.NonEmpty ((ProductType,Block), Block)
type Routes = [((ProductType,Block), Block)]

--
-- Routing.hs ends here
