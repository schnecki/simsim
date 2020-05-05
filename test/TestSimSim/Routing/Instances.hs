{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- Instances.hs ---
--
-- Filename: Instances.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 22:47:46 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 9
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


module TestSimSim.Routing.Instances (sizedRouting, sizedRoutes) where

import           Control.Monad
import qualified Data.List.NonEmpty               as NL
import qualified Data.Map.Strict                  as M
import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.ProductType
import           SimSim.Routing

import           TestSimSim.Block.Instances
import           TestSimSim.ProductType.Instances


sizedRouting :: Gen Routing
sizedRouting = NL.fromList <$> sizedRoutes


sizedRoutes :: Gen Routes
sizedRoutes = sized $ \n -> do
      let bls = sizedBlocks n
          machines = filter isMachine bls
          products = sizedProductTypes n
      shuffledPts <- shuffle products
      prds <- (head shuffledPts:) <$>  sublistOf (tail shuffledPts)
      let mkRouting prd ms = (\(xs, lastX) -> xs ++ [((prd, lastX), FGI)]) $ foldl mkRoute ([], OrderPool) ms
            where mkRoute :: ([((ProductType,Block), Block)],Block) -> Block -> ([((ProductType,Block), Block)],Block)
                  mkRoute (acc, lastX) ms@(Machine n) = (acc ++ [((prd, lastX), Queue n), ((prd, Queue n), ms)], ms)
      routes <- mapM (\p -> sublistOf machines >>= shuffle >>= return . mkRouting p) products
      return $ concat routes

--
-- Instances.hs ends here
