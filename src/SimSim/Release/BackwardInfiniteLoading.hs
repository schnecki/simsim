-- BackwardInfiniteLoading.hs ---
--
-- Filename: BackwardInfiniteLoading.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Jun 25 11:00:25 2019 (+0200)
-- Version:
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


module SimSim.Release.BackwardInfiniteLoading
    ( releaseBIL
    ) where

import           ClassyPrelude
import qualified Data.Map.Strict     as M

import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Release.Type
import           SimSim.Time


releaseBIL :: M.Map ProductType Time -> Release
releaseBIL m = Release (bilRelease m) (name m)

bilRelease :: M.Map ProductType Time -> Time -> [Order] -> IO [Order]
bilRelease m t = return . filter bil
  where
    bil order =
      case M.lookup (productType order) m of
        Nothing -> error $ "Product  " ++ show (productType order) ++ " was not found in the map specified for releaseBIL."
        Just delta -> dueDate order - delta <= t

name :: M.Map ProductType Time -> Text
name m = "BackwardInfiniteLoading" ++ tshow (M.toList m) ++ ""


--
-- BackwardInfiniteLoading.hs ends here
