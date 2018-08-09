{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- RoutingSpec.hs ---
--
-- Filename: RoutingSpec.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Thu Aug  9 08:49:56 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 23
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

module SimSim.RoutingSpec (spec, sizedRouting, sizedRoutes) where

import           Control.Monad
import qualified Data.List.NonEmpty     as NL
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Prelude
import           Test.Hspec
import           Test.QuickCheck

import           SimSim.Block
import           SimSim.ProductType
import           SimSim.Routing

import           SimSim.BlockSpec       hiding (spec)
import           SimSim.ProductTypeSpec hiding (spec)

-- instance Arbitrary Routing where
--   arbitrary =

sizedRouting :: Gen Routing
sizedRouting = NL.fromList <$> sizedRoutes


sizedRoutes :: Gen Routes
sizedRoutes = sized $ \n -> do
      let bls = sizedBlocks n
          machines = filter isMachine bls
      prds <- sublistOf $ sizedProductTypes n
      let mkRouting prd ms = (\(xs, last) -> xs ++ [((prd, last), FGI)]) $ foldl mkRoute ([], OrderPool) ms
            where mkRoute :: ([((ProductType,Block), Block)],Block) -> Block -> ([((ProductType,Block), Block)],Block)
                  mkRoute (acc, last) ms@(Machine n) = (acc ++ [((prd, last), Queue n), ((prd, Queue n), ms)], ms)
      routes <- mapM (\p -> sublistOf machines >>= shuffle >>= return . mkRouting p) prds
      return $ concat routes

spec :: Spec
spec = return ()

--
-- RoutingSpec.hs ends here
