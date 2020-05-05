{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
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
--     Update #: 151
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
import           Control.DeepSeq
import           GHC.Exts          (Constraint)

import           Data.Dynamic
import           Data.Proxy
import           Data.Typeable
import           SimSim.Block
import           SimSim.Order.Type


type DispatchFunction = Block -> [Order] -> [Order]

data Dispatch = Dispatch
  { dispatcher         :: !DispatchFunction
  , uniqueDispatchName :: !Text
  } deriving (Generic, NFData)

instance Eq Dispatch where
  (Dispatch _ n1) == (Dispatch _ n2) = n1 == n2

instance Ord Dispatch where
  compare (Dispatch _ n1) (Dispatch _ n2) = compare n1 n2


instance Show Dispatch where
  show = unpack . uniqueDispatchName


--
-- Type.hs ends here
