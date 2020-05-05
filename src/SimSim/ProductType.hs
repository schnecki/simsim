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
--     Update #: 13
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
import           Control.DeepSeq
import           Data.Serialize
import           GHC.Generics


newtype ProductType =
  Product Int                   -- ^ Product types must start with 1.
  deriving (Show, Read, Eq, Ord, Generic, Serialize, NFData)

productTypeNr :: ProductType -> Int
productTypeNr (Product nr) = nr


--
-- ProductType.hs ends here
