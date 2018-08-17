-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  1 13:58:51 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 81
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

module SimSim.Simulation.Pretty
    ( prettySimulation
    , prettySimSim
    ) where

import           ClassyPrelude                hiding (empty, (</>), (<>))
import qualified Data.Map.Strict              as M
import           Text.PrettyPrint.ANSI.Leijen

import           SimSim.Block
import           SimSim.Order
import           SimSim.Simulation.Type
import           SimSim.Statistics.Pretty
import           SimSim.Time

instance Pretty SimSim where
  pretty = prettySimulation True False prettyOrderDue


prettySimSim :: SimSim -> Text
prettySimSim sim = pack $ displayS (renderSmart 1.0 97 $ pretty sim) ""

prettySimulation :: Bool -> Bool -> (Order -> Doc) -> SimSim -> Doc
prettySimulation isTest updateStats prettyOrder sim =
  nest 2 $
  text "Simulation:" <$$>
  text "Next OrderId:" <+> integer (simNextOrderId sim) <$$>
  text "Current Simulation Time:" <+> prettyTime (simCurrentTime sim) <$$>
  text "Period Length:" <+> prettyTime (simPeriodLength sim) <+> "steps" <$$>
  nest 2 (prettyOrderPool prettyOrder (simOrdersOrderPool sim)) <$$>
  nest 2 (prettyOrderQueue isTest updateStats prettyOrder (simOrdersQueue sim)) <$$>
  nest 2 (prettyOrderMachine prettyOrder (simOrdersMachine sim)) <$$>
  nest 2 (prettyOrderFgi prettyOrder (simOrdersFgi sim)) <$$>
  nest 2 (prettyOrderFinishedOrders prettyOrder (simOrdersShipped sim)) <$$>
  nest 2 (prettySimStatistics isTest updateStats sim) <$$> empty


prettyOrderPool :: (Order -> Doc) -> [Order] -> Doc
prettyOrderPool prettyOrder xs =
  text "Order Pool:" <$$> prettyOrderList prettyOrder xs

prettyOrderQueue :: Bool -> Bool -> (Order -> Doc) -> M.Map Block [Order] -> Doc
prettyOrderQueue isTest updateStats prettyOrder m =
  text "Queues:" <$$> prettyMap (text . show) (prettyOrderList prettyOrder) m

  where m' | not isTest && updateStats = M.fromList $ concatMap toPair $ M.elems m
           | otherwise = m
        toPair []     = []
        toPair (x:xs) = [(lastBlock x, (x:xs))]

prettyMap :: (a -> Doc) -> (t -> Doc) -> Map a t -> Doc
prettyMap prettyKey prettyValue m =
  vcat $ map (\(k,v) -> nest 2 $ prettyKey k <> ":" </> prettyValue v) (M.toList m)

prettyOrderMachine :: (Order -> Doc) -> Map Block (Order, Time) -> Doc
prettyOrderMachine prettyOrder m =
  text "Machines: " <$$> prettyMap (text . show) (\(o,t) -> nest 2 $ text "still needs processing for" <+> prettyTime t <> " steps:" </> prettyOrder o) m

prettyOrderFgi :: (Order -> Doc) -> [Order] -> Doc
prettyOrderFgi prettyOrder xs =
  text "Finished Goods Inventory:" <$$> prettyOrderList prettyOrder xs


prettyOrderFinishedOrders :: (Order -> Doc) -> [Order] -> Doc
prettyOrderFinishedOrders prettyOrder xs =
  text "Shipped Orders:" <$$> prettyOrderList prettyOrder xs


prettyOrderList :: (Order -> Doc) -> [Order] -> Doc
prettyOrderList prettyOrder [] = empty
prettyOrderList prettyOrder xs = fillCat $ intersperse (text ", ") $ map prettyOrder xs


--
-- Pretty.hs ends here
