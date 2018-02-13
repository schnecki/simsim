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
--     Update #: 53
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

import           SimSim


routing :: Routes
routing = [(Product 1, Source) --> Machine 1
          ,(Product 2, Source) --> Machine 2
          ,(Product 2, Machine 2) --> Machine 1
          ]


-- | Order to send through the production
incomingOrders :: [Order]
incomingOrders = L.concat $ L.replicate 10 [
  newOrder (Product 1) 1 2,
  newOrder (Product 2) 1 2,
  newOrder (Product 1) 1 2,
  newOrder (Product 1) 1 2]


-- | Compose the example messages with ratelimiting and a pipe that prints each
-- message received.
main :: IO ()
main = do
  let sim = newSimSim routing
  simulate sim incomingOrders


-- mex = runEffect $ each ["Test", "ABC"] >-> example

-- example :: (Show a) => Consumer a IO r
-- example = flip evalStateT 0 $ forever $ do
--     -- Inside here we are using `StateT Int (Consumer a IO) r`
--     a <- lift await
--     n <- get
--     lift $ lift $ putStrLn $ "Received value #" ++ show n ++ ": " ++ show a
--     put (n + 1)

-- mex2 :: IO ()
-- mex2 = runEffect $ evalStateP 0 $ each ["Test", "ABC"] >-> example2 >-> example3


-- example3 :: (Show a) => Consumer a (StateT Integer IO) r
-- example3 = forever $ do
--     -- Inside here we are using `StateT Int (Consumer a IO) r`
--     a <- await
--     n <- lift get
--     lift $ lift $ putStrLn $ "Ex3 #" ++ show n ++ ": " ++ show a
--     lift $ put (n * 3)

-- example2 :: (Show a) => Pipe a a (StateT Integer IO) r
-- example2 = forever $ do
--     -- Inside here we are using `StateT Int (Consumer a IO) r`
--     a <- await
--     n <- lift get
--     lift $ lift $ putStrLn $ "Ex2 #" ++ show n ++ ": " ++ show a
--     lift $ put (n + 1)
--     yield a


-- main2 = runPipe $ yield () >+> transPipe (flip evalStateT 0) (do
--     a <- lift get
--     lift $ put $ a + 1
--     b <- lift get
--     liftIO $ print (a, b)

--     _ <- await

--     (x, y) <- lift $ do
--         x <- get
--         put $ x + 1
--         y <- get
--         return (x, y)
--     liftIO $ print (x, y))


--
-- Main.hs ends here
