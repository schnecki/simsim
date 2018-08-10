{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Jul 29 09:14:44 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 146
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

module SimSim.Dispatch.Type where

import           ClassyPrelude
import           GHC.Exts          (Constraint)

import           Data.Dynamic
import           Data.Proxy
import           Data.Typeable
import           SimSim.Block
import           SimSim.Order.Type


type DispatchFunction = Block -> [Order] -> [Order]

data Dispatch = Dispatch
  { dispatcher :: DispatchFunction
  , uniqueName :: Text
  }

instance Show Dispatch where
  show = unpack . uniqueName


--
-- Type.hs ends here
