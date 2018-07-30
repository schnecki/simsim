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
--     Update #: 150
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
  , (Product 1, Machine 1) --> Queue 2
  , (Product 1, Queue 2) --> Machine 2
  , (Product 1, Machine 2) --> FGI

  , (Product 2, OrderPool) --> Queue 2   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 2)   --> Machine 2 -- note: dispatching to sink is implicit
  , (Product 2, Machine 2) --> Queue 1
  , (Product 2, Queue 1)   --> Machine 1
  , (Product 2, Machine 1) --> FGI
  ]

periodLen :: Time
periodLen = 10

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, const 4)
                        ,(Product 2, const 2)])
            ,(Machine 2,[(Product 1, const 2)
                        ,(Product 2, const 4)])
            ]


-- | Order to send through the production
incomingOrders :: [Order]
incomingOrders = L.concat $ L.replicate 1
  [ newOrder (Product 1) 0 1
  , newOrder (Product 2) 0 1
  , newOrder (Product 1) 0 1
  , newOrder (Product 1) 0 1
  ]


measure :: (MonadIO m) => m a -> m a
measure e = do
  !start <- liftIO Time.getCurrentTime
  !x <- e
  !stop <- liftIO Time.getCurrentTime
  putStrLn $ "\nRuntime: " ++ tshow (Time.diffUTCTime stop start)
  return x

main :: IO ()
main =
  measure $ do
    g <- newStdGen
    let sim = newSimSim g routing procTimes periodLen immediateRelease (prioritizeProducts [Product 2]) -- firstComeFirstServe
    sim' <- foldM simulate sim ([incomingOrders] ++ replicate 00 [])
    putStrLn $ "\n\nProduct routes: " ++ tshow (simProductRoutes $ simInternal sim')
    putStrLn $ "OP: " ++ tshow (fmap orderId $ simOrderPoolOrders $ simInternal sim')
    putStrLn $ "Queues: " ++ tshow (M.map (fmap orderId) $ simOrdersQueue $ simInternal sim')
    putStrLn $ "Machines: " ++ tshow (fmap (first orderId) $ simOrdersMachine $ simInternal sim')
    putStrLn $ "FGI: " ++ tshow (-- fmap orderId $
                                 simOrdersFgi $ simInternal sim')
    putStrLn $ "Finished: " ++ tshow (map orderId $ simFinishedOrders sim')
    putStrLn $ "Block times: " ++ tshow (simBlockTimes $ simInternal sim')
    print $ simBlockLastOccur $ simInternal sim'

--
-- Main.hs ends here
