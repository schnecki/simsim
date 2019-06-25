{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Fri Aug 10 19:11:37 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 18
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

module SimSim.Release.Type
    ( Release (..)
    , ReleaseFun
    ) where

import           ClassyPrelude
import           Data.Serialize

import           SimSim.Order.Type
import           SimSim.Time


type ReleaseFun = Time -> [Order] -> IO [Order]


data Release = Release
  { releaser          :: ReleaseFun
  , uniqueReleaseName :: Text
  } deriving (Generic, NFData)

instance Eq Release where
  (Release _ n1) == (Release _ n2) = n1 == n2

instance Ord Release where
  compare (Release _ n1) (Release _ n2) = compare n1 n2

instance Show Release where
  show = unpack . uniqueReleaseName


--
-- Type.hs ends here
