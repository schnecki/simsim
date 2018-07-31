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
--     Update #: 26
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
import           Data.Ratio
import           Text.Printf

newtype Time = Time Rational
  deriving (Ord, Eq)

instance Num Time where
  fromInteger x = Time (x % 1)
  Time x + Time y = Time (x + y)
  Time x - Time y = Time (x - y)
  Time x * Time y = Time (x * y)
  abs (Time x) = Time (abs x)
  signum (Time x) = Time (signum x)

instance Fractional Time where
  fromRational x = Time $ toRational x
  (Time x) / (Time y) = Time (x/y)

instance Show Time where
  show (Time t) = "t=" ++ printf "%.3f" (fromRational t :: Double)

type CurrentTime = Time

-- Synonyms
type ArrivalDate = Time
type DueDate = Time


--
-- Time.hs ends here
