{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- ProductType.hs ---
--
-- Filename: ProductType.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 16:22:43 2017 (+0100)
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

module SimSim.ProductType where

import           ClassyPrelude
import           Data.Serialize
import           GHC.Generics


newtype ProductType = Product Int deriving (Show, Eq, Ord, Generic, Serialize)


--
-- ProductType.hs ends here
