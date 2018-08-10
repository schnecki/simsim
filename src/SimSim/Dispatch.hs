{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
-- Dispatch.hs ---
--
-- Filename: Dispatch.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Sun Jul 29 09:14:44 2018 (+0200)
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

module SimSim.Dispatch
  ( module SimSim.Dispatch.Type
  , module FCFS
  -- , Dispatch
  ) where

import           ClassyPrelude

import           SimSim.Block
import           SimSim.Dispatch.FirstComeFirstServe as FCFS
import           SimSim.Dispatch.Type
import           SimSim.Order.Type


--
-- Dispatch.hs ends here
