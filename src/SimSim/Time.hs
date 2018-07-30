{-# LANGUAGE TypeSynonymInstances #-}
-- Time.hs ---
--
-- Filename: Time.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 16:51:22 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 15
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

module SimSim.Time where

import           ClassyPrelude

newtype Time = Time Integer
  deriving (Ord, Eq)

instance Num Time where
  fromInteger = Time
  Time x + Time y = Time (x + y)
  Time x - Time y = Time (x - y)
  Time x * Time y = Time (x * y)
  abs (Time x) = Time (abs x)
  signum (Time x) = Time (signum x)

instance Show Time where
  show (Time t) = "t=" ++ show t


type CurrentTime = Time


-- Synonyms
type ArrivalDate = Time
type DueDate = Time


--
-- Time.hs ends here
