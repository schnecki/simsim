{-# LANGUAGE TemplateHaskell #-}
-- Runner.hs ---
--
-- Filename: Runner.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Nov  1 17:00:27 2017 (+0100)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 218
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

module SimSim.Runner.Runner
    ( simulate
    , simulateUntil
    , simulateLogging
    , simulateUntilLogging
    ) where

import           ClassyPrelude              hiding (replicate)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.State.Strict
import           Control.Monad.Trans.Class
import qualified Data.List                  as L
import qualified Data.Map.Strict            as M
import           Data.Monoid                ((<>))
import           Data.Sequence              (replicate)
import           Data.Text                  (Text)
import           Data.Void
import           Debug.Trace
import           Pipes
import           Pipes.Core
import           Pipes.Lift
import qualified Pipes.Prelude              as Pipe
import qualified Prelude                    as Prelude
import           System.Random


import           SimSim.Block
import           SimSim.Order
import           SimSim.Order.Type
import           SimSim.ProductType
import           SimSim.Release
import           SimSim.Routing
import           SimSim.Simulation.Ops
import           SimSim.Simulation.Type
import           SimSim.Statistics.Ops
import           SimSim.Statistics.Pretty
import           SimSim.Time


import           SimSim.Runner.Dispatch
import           SimSim.Runner.Fgi
import           SimSim.Runner.Machine
import           SimSim.Runner.Queue
import           SimSim.Runner.Release
import           SimSim.Runner.Server
import           SimSim.Runner.Sink
import           SimSim.Runner.Util

-- | This function simulates the production system. The simulation pipelines are split into to seperate pipes. The first
-- one feds the orders into the system, whereas the second one releases the orders from the order pool and feds it into
-- the production system.
simulation :: (MonadLogger m, MonadIO m) => SimSim -> Time -> [Order] -> Proxy X () () X m SimSim
simulation sim simEnd incomingOrders = do
  let timeNextPeriod = simCurrentTime sim + simPeriodLength sim
  let stSim = setSimEndTime (min timeNextPeriod simEnd) sim
  simOp <- execStateP stSim (mkPipeOrderPool stSim incomingOrders)
  let opOrders = simOrdersOrderPool simOp
      time = simCurrentTime simOp
      arrivedOpOrders = filter ((<= time) . arrivalDate) opOrders
  relOrds <- liftIO $ releaser (simRelease simOp) (simCurrentTime simOp) arrivedOpOrders
  let simRel = removeOrdersFromOrderPool relOrds simOp
  sim' <- finalize <$> execStateP simRel (mkPipeProdSys simRel relOrds)
  if simEnd > simCurrentTime sim'
    then simulation sim' simEnd []
    else return sim'
  where
    finalize sim = statsEndPeriodAddCosts $ mapBlockTimes (max endT) $ fixIdleTimeQueues $ fixIdleTimeFgi $ setSimCurrentTime endT sim
      where endT = simEndTime $ simInternal sim
    dummyOrder = (newOrder (Product 1) simEnd simEnd) {orderCurrentTime = simEnd, blockStartTime = simEnd}
    fixIdleTimeQueues sim = foldl' (\s bl -> statsAddBlockPartialUpdate ProcTime bl dummyOrder s) sim blsQueues
      where
        blsMachines = filter (\bl -> isMachine bl) (toList $ simBlocks $ simInternal sim) -- queue is idle
        blsQueues = L.nub $ concatMap (dispatchReverse (simRouting sim)) $ filter (\mBl -> maybe True null (M.lookup mBl (simOrdersQueue sim))) blsMachines
    fixIdleTimeFgi sim
      | null (simOrdersFgi sim)
                           -- trace ("Adding dummy order. Before: " ++ show (prettySimStatistics True sim))
                           -- trace ("Afterwards: " ++ show (prettySimStatistics True $ statsAddBlock ProcTime FGI dummyOrder sim))
       = statsAddBlockPartialUpdate ProcTime FGI dummyOrder sim
      | otherwise = sim

-- | This function simulates the system. For this the incoming orders are first put into the order pool and once
-- released they are fed into the production system. The simulation halts after one period.
simulate :: SimSim -> [Order] -> IO SimSim
simulate sim incomingOrders = do
  let simEnd = simCurrentTime sim + simPeriodLength sim
  runNoLoggingT $ runEffect $ simulation sim simEnd incomingOrders


-- | This function simulates the system. For this the incoming orders are first put into the order pool and once
-- released they are fed into the production system. The simulation halts after one period. The specified logging
-- function from `Control.Monad.Logger` is applied.
simulateLogging :: (LoggingT IO SimSim -> IO SimSim) -> SimSim -> [Order] -> IO SimSim
simulateLogging loggingFun sim incomingOrders = do
  let simEnd = simCurrentTime sim + simPeriodLength sim
  simulateLoggingEnd loggingFun simEnd sim incomingOrders


-- | This function simulates the system. For this the incoming orders are first put into the order pool and once
-- released they are fed into the production system. The simulation halts after one period.
simulateLoggingEnd :: (LoggingT IO SimSim -> IO SimSim) -> Time -> SimSim -> [Order] -> IO SimSim
simulateLoggingEnd loggingFun simEnd sim incomingOrders = loggingFun $ runEffect $ simulation sim simEnd incomingOrders


-- | This function simulates the system. For this the incoming orders are first put into the order pool and once
-- released they are fed into the production system. The simulation halts at the specified end time.
simulateUntil :: Time -> SimSim -> [Order] -> IO SimSim
simulateUntil simEnd sim incomingOrders = runNoLoggingT $ runEffect $ simulation sim simEnd incomingOrders


-- | This function simulates the system. For this the incoming orders are first put into the order pool and once
-- released they are fed into the production system. The simulation halts at the specified end time. The specified
-- logging function from `Control.Monad.Logger` is applied.
simulateUntilLogging :: (LoggingT IO SimSim -> IO SimSim) -> Time -> SimSim -> [Order] -> IO SimSim
simulateUntilLogging loggingFun simEnd sim incomingOrders = simulateLoggingEnd loggingFun simEnd sim incomingOrders


-------------------- Helper functions --------------------


-- | This function creates the pipe which serves the orders into the order pool.
mkPipeOrderPool :: (MonadLogger m, MonadIO m) => SimSim -> [Order] -> Proxy X () () X (StateT SimSim m) ()
mkPipeOrderPool sim incomingOrders = server sim incomingOrders >>~ orderPoolSink


-- | This function creates the pipe which simulates the production system.
mkPipeProdSys :: (MonadLogger m, MonadIO m) => SimSim -> [Order] -> Proxy X () () X (StateT SimSim m) ()
mkPipeProdSys sim ordersToRelease =
  release sim routes ordersToRelease >>~
  foldl' -- repeat machine & queues often enough
    (>~>)
    (queue "QPipe1" routes >~> machine "MPipe1" routes)
    (map (\x -> queue ("QPipe" ++ tshow x) routes >~> machine ("MPipe" ++ tshow x) routes) [2 .. maxMs + 1]) >~>
  fgi >~>
  sink
  where
    maxMs = simMaxMachines $ simInternal sim
    lastOccur = Prelude.maximum $ M.elems (simBlockLastOccur $ simInternal sim)
    routes = simRouting sim

--
-- Runner.hs ends here
