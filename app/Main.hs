{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE NoImplicitPrelude #-}
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
--     Update #: 273
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
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import qualified Data.List                        as L
import qualified Data.Map.Strict                  as M
import           Data.Monoid                      ((<>))
import           Data.Ratio
import           Data.Text                        (Text)
import qualified Data.Time                        as Time
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude                    as Pipe
import           System.Random
import           Text.PrettyPrint.ANSI.Leijen     (putDoc)


import           SimSim
import           SimSim.Order.Type
import           SimSim.Simulation.Type


routing :: Routes
routing =
  [ (Product 1, OrderPool) --> Queue 1   -- source -> 1 -> 2 -> sink
  , (Product 1, Queue 1)   --> Machine 1 -- note: route to sink is not necessary
  , (Product 1, Machine 1) --> Queue 2
  , (Product 1, Queue 2)   --> Machine 2
  , (Product 1, Machine 2) --> FGI

  , (Product 2, OrderPool) --> Queue 2   -- source -> 2 -> 1 -> sink
  , (Product 2, Queue 2)   --> Machine 2 -- note: route to sink is not necessary
  , (Product 2, Machine 2) --> Queue 1
  , (Product 2, Queue 1)   --> Machine 1
  , (Product 2, Machine 1) --> FGI
  ]

periodLen :: Time
periodLen = 1

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, return . const 0.15)
                        ,(Product 2, return . const 0.15)])
            ,(Machine 2,[(Product 1, return . const 0.15)
                        ,(Product 2, return . const 0.15)])
            ]


-- | Order to send through the production
incomingOrders :: [Order]
incomingOrders = L.concat $ L.replicate 1
  [ newOrder (Product 1) 0 10
  , newOrder (Product 2) 0 10
  , newOrder (Product 1) 0 10
  , newOrder (Product 1) 0 10
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
    sim <- newSimSimIO routing procTimes periodLen releaseImmediate dispatchFirstComeFirstServe shipOnDueDate
    -- sim'' <- foldM (simulateLogging runStderrLoggingT) sim ([incomingOrders] ++ replicate 1 [])
    sim' <- simulateUntilLogging runStderrLoggingT 0.833 sim incomingOrders
    let runSim !s 0 = return s
        runSim !s p = do
          ords <- generateOrdersUniform s 1 11 10
          !s' <- simulate s ords
          runSim s' (p - 1)
    sim'' <- runSim sim' 1000

    -- sim'' <- simulateUntil 11000 sim' []
    -- sim'' <- simulateUntilLogging runStderrLoggingT 3 sim' [] -- incomingOrders
    -- putStrLn $ "\n\nProduct routes: " ++ tshow (simProductRoutes $ simInternal sim')
    -- putStrLn $ "OP: " ++ tshow (fmap orderId $ simOrderPoolOrders sim')
    -- putStrLn $ "Queues: " ++ tshow (M.map (fmap orderId) $ simOrdersQueue sim')
    -- putStrLn $ "Machines: " ++ tshow (fmap (first orderId) $ simOrdersMachine sim')
    -- putStrLn $ "FGI: " ++ tshow (-- fmap orderId $
    --                              simOrdersFgi sim')
    -- putStrLn $ "Finished: " ++ tshow (map orderId $ simOrdersFinished sim')
    -- putStrLn $ "Block times: " ++ tshow (simBlockTimes $ simInternal sim')
    -- putDoc $ prettySimStatistics (simStatistics sim')
    -- putStrLn $ prettySimSim sim'
    putStrLn $ prettySimSim sim''

--
-- Main.hs ends here
