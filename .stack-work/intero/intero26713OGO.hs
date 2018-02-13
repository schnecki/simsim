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
--     Update #: 64
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
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude                    as Pipe
import           System.Random

import           SimSim


routing :: Routes
routing = [(Product 1, Source) --> Machine 1
          ,(Product 2, Source) --> Machine 2
          ,(Product 2, Machine 2) --> Machine 1
          ]

periodLen :: Integer
periodLen = 960

procTimes :: ProcTimes
procTimes = [(Machine 1,[(Product 1, const 96)
                        ,(Product 2, const 96)])
            ,(Machine 2,[(Product 1, error "not possible")
                        ,(Product 2, const 96)])
            ]


-- | Order to send through the production
incomingOrders :: [Order]
incomingOrders = L.concat $ L.replicate 10 [
  newOrder (Product 1) 1 2,
  newOrder (Product 2) 1 2,
  newOrder (Product 1) 1 2,
  newOrder (Product 1) 1 2]


main :: IO ()
main = do
  g <- newStdGen
  let sim = newSimSim g routing procTimes periodLen
  simulate sim incomingOrders


--
-- Main.hs ends here
