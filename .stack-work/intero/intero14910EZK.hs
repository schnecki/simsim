{-# LANGUAGE AllowAmbiguousTypes #-}
-- Type.hs ---
--
-- Filename: Type.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 20:28:23 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 87
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

module SimSim.Simulation.Type where

import           ClassyPrelude
-- import           Foundation

import qualified Data.List.NonEmpty as NL

import           SimSim.Block
import           SimSim.ProductType
import           SimSim.Routing
import           SimSim.Time


data SimSim =
  SimSim { simRouting     :: Routing
         , simBlocks      :: NL.NonEmpty Block
         , simCurrentTime :: Time
         , simInternal    :: SimInternal
         }


data SimInternal =
  SimInternal { simStartTime :: Time
              , simEndTime   :: Time
              , maxMachines  :: Int
              }


newSimSim :: Routes -> SimSim
newSimSim routesE = case NL.nonEmpty routesE of
  Nothing -> error "Routing cannot be empty"
  Just routes -> SimSim routes uniqueBlocks 0 (SimInternal 0 0 maxMachines)
    where uniqueBlocks = NL.fromList $ fmap NL.head $ NL.group $ NL.sort allBlocks
          allBlocks = fmap snd routes <> fmap (snd.fst) routes
          maxMachines = maximum (impureNonNull $ 1:lengths )
          lengths = fmap length $ NL.group $
                    NL.sortBy (compare `on` id) $ fmap (fst.fst) routes


-- group :: (Eq (Item c), Foldable c, Sequential c) =>
--          NonEmpty c -> NonEmpty [NonEmpty [Item c]]
-- group = groupBy ((==) `on` id)

-- groupBy :: (Sequential c, Foldable c) =>
--            (Item c -> Item c -> Bool) -> NonEmpty c -> NonEmpty [NonEmpty [Item c]]
-- groupBy f xs = fromMaybe (error "groupBy was empty 1") $ nonEmpty $
--                fmap (fromMaybe (error "groupBy was empty") . nonEmpty) $
--                fst $ foldl' (\(ac:acc,bef) x ->
--                                if bef `f` x
--                                then ((x:ac):acc,x)
--                                else ([x]:ac:acc,x))
--                ([singleton (head xs)],head xs) xs


    -- instance Foldable c => Foldable (NonEmpty c) where
    --   foldl' f a (NonEmpty xs) = foldl' f a xs
    --   foldr' f a (NonEmpty xs) = foldr' f a xs


--
-- Type.hs ends here
