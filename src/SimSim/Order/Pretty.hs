{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-- Pretty.hs ---
--
-- Filename: Pretty.hs
-- Description:
-- Author: Manuel Schneckenreither
-- Maintainer:
-- Created: Wed Aug  1 15:08:50 2018 (+0200)
-- Version:
-- Package-Requires: ()
-- Last-Updated:
--           By:
--     Update #: 15
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

module SimSim.Order.Pretty where


import           ClassyPrelude                (maybe, show, ($), (.))
import           Text.PrettyPrint.ANSI.Leijen

import           SimSim.Order.Type
import           SimSim.Time

prettyOrderId :: Order -> Doc
prettyOrderId order = text "Order" <+> integer (orderId order)

prettyOrderDue :: Order -> Doc
prettyOrderDue order = prettyOrderId order <+> parens (text (show $ productType order) <> comma <+> text "due:" <+> prettyTime (dueDate order))

prettyOrder :: Order -> Doc
prettyOrder order =
  nest 2 (text "Order" <+> integer (orderId order)) <$$>
  text "Product Type:" <+> text (show $ productType order) <$$>
  text "Arrival and Due Date:" <+> prettyTime (arrivalDate order) <+> "--" <+> prettyTime (dueDate order) <$$>
  maybe empty ((text "Released:" <+>) . prettyTime) (released order) <$>
  maybe empty ((text "ProdStart:" <+>) . prettyTime) (prodStart order) <$>
  maybe empty ((text "ProdEnd:" <+>) . prettyTime) (prodEnd order) <$>
  maybe empty ((text "Shipped:" <+>) . prettyTime) (shipped order)


--
-- Pretty.hs ends here
