{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
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
--     Update #: 24
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
import qualified Data.List.NonEmpty as NL
import qualified Data.Map.Strict    as M
import           Data.Serialize
import           GHC.Generics
import           System.Random

-- Idea: recursive Block datatype? to model full work centers etc.
data Block
  = OrderPool                   -- ^. This block feds the orders into the system.
  | Queue Int                   -- ^. This block queues the orders until dispatched.
  | Machine Int                 -- ^. This block is a machine.
  | FGI                         -- ^. This block is a finished goods inventory.
  | Sink                        -- ^. This block removes the orders from the system.
  deriving (Show, Eq, Ord, Generic, Serialize)

isMachine :: Block -> Bool
isMachine Machine{} = True
isMachine _         = False

isQueue :: Block -> Bool
isQueue Queue{} = True
isQueue _       = False


isFgi :: Block -> Bool
isFgi FGI{} = True
isFgi _     = False

isOrderPool :: Block -> Bool
isOrderPool OrderPool{} = True
isOrderPool _           = False

isSink :: Block -> Bool
isSink Sink{} = True
isSink _      = False


--
-- Block.hs ends here
