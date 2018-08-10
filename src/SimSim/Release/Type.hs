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
--     Update #: 6
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
    , ReleaseFun (..)
    ) where

import           ClassyPrelude

import           SimSim.Order
import           SimSim.Time


type ReleaseFun = Time -> [Order] -> IO [Order]


data Release = Release
  { releaser          :: ReleaseFun
  , uniqueReleaseName :: Text
  }

instance Show Release where
  show = unpack . uniqueReleaseName

-- instance Read Release where
--   readsPrec =


--
-- Type.hs ends here
