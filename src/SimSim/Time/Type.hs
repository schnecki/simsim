-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  1 14:36:56 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 8
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

module SimSim.Time.Type where


import           ClassyPrelude
import           Data.Ratio
import           Text.Printf

-- Synonyms
type CurrentTime = Time
type ArrivalDate = Time
type DueDate = Time


newtype Time = Time Rational
  deriving (Ord, Eq, Show, Read)

fromTime :: Time -> Rational
fromTime (Time t) = t

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


--
-- Type.hs ends here
