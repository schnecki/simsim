{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
-- Main.hs ---
--
-- Filename: Main.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Tue Oct 31 22:19:48 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 97
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


module Main where

import           ClassyPrelude
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Time                        as Time
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude                    as Pipe
import           System.Random

import           SimSim
import           SimSim.Order.Type
import           SimSim.Simulation.Type


routing :: Routes
routing =
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> sink
  , (Product 1, Queue 1)   --> Machine 1

  , (Product 2, OrderPool) --> Queue 2   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 2)   --> Machine 2 -- note: dispatching to sink is implicit
  , (Product 2, Machine 2) --> Queue 1
  , (Product 2, Queue 1)   --> Machine 1
  ]

periodLen :: Integer
periodLen = 10

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, const 1)
                        ,(Product 2, const 1)])
            ,(Machine 2,[-- (Product 1, error "not possible")
                        (Product 2, const 1)])
            ]


-- | Order to send through the production
incomingOrders :: [Order]
incomingOrders = L.concat $ L.replicate 1 [
  newOrder (Product 1) 1 2,
  newOrder (Product 2) 1 2,
  newOrder (Product 1) 1 2,
  newOrder (Product 1) 1 2]


measure :: (MonadIO m) => m a -> m a
measure e = do
  !start <- liftIO Time.getCurrentTime
  !x <- e
  !stop <- liftIO Time.getCurrentTime
  putStrLn $ "\nRuntime: " ++ tshow (Time.diffUTCTime stop start)
  return x

main :: IO ()
main = measure $ do
  g <- newStdGen
  let sim = newSimSim g routing procTimes periodLen immediateRelease firstComeFirstServe
  sim' <- simulate sim incomingOrders
  print $ "OP: " ++ show (simOrderPoolOrders $ simInternal sim')
  print $ "Queues: " ++ show (M.map (fmap orderId) $ simQueueOrders $ simInternal sim')
  print $ "Block times: " ++ show (simBlockTimes $ simInternal sim')


--
-- Main.hs ends here
