-- BlockSpec.hs ---
--
-- Filename: BlockSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:54:00 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 1
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


module TestSimSim.Block.BlockSpec (spec) where

import qualified Data.Map.Strict            as M
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block

import           TestSimSim.Block.Instances

spec :: Spec
spec = do
  describe "Block type properties" $
    do it "prop_isMachine" $ property prop_isMachine

prop_isMachine :: Block -> Bool
prop_isMachine bl@Machine{} = isMachine bl == True
prop_isMachine bl           = isMachine bl == False


--
-- BlockSpec.hs ends here
