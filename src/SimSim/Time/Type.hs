{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
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

module SimSim.Time.Type where


import           ClassyPrelude
import           Control.DeepSeq
import           Data.Ratio
import           Data.Serialize
import           GHC.Generics
import           Text.Printf


-- Synonyms
type CurrentTime = Time
type ArrivalDate = Time
type DueDate = Time


newtype Time = Time Rational
  deriving (Ord, Eq, Generic, Serialize, NFData)

instance Show Time where
  show (Time x) = printFloat (fromRational x)
    where
      printFloat :: Double -> String
      printFloat = printf ("%." ++ show commas ++ "f")
      commas = 2 :: Int

fromTime :: Time -> Rational
fromTime (Time t) = t

timeFromDouble :: Double -> Time
timeFromDouble = Time . toRational

timeToDouble :: Time -> Double
timeToDouble = fromRational . fromTime

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

instance Real Time where
  toRational (Time x) = x

instance RealFrac Time where
  properFraction (Time x) = (a, Time b)
    where (a,b) = properFraction x

instance Enum Time where
  toEnum x = Time $ toEnum x
  fromEnum (Time x) = fromInteger (truncate x)

--
-- Type.hs ends here
