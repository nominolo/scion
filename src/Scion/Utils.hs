{-# OPTIONS_GHC -fno-warn-orphans #-}
-- |
-- Module      : Scion.Utils
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Various utilities.
--
module Scion.Utils where

import Scion.Types

import GHC              ( GhcMonad, ModSummary, spans, getLoc, Located
                        , depanal, topSortModuleGraph, TypecheckedMod
                        , mkPrintUnqualifiedForModule, moduleInfo )
import Digraph          ( flattenSCCs )
import Bag              ( Bag, mapBag, foldrBag, foldlBag )
import Outputable

import Control.Monad
import Data.Foldable
import Data.Maybe ( fromMaybe )

thingsAroundPoint :: (Int, Int) -> [Located n] -> [Located n]
thingsAroundPoint pt ls = [ l | l <- ls, spans (getLoc l) pt ]

modulesInDepOrder :: GhcMonad m => m [ModSummary]
modulesInDepOrder = do
  gr <- depanal [] False
  return $ flattenSCCs $ topSortModuleGraph False gr Nothing
  
-- in dep-order
foldModSummaries :: GhcMonad m =>
                    (a -> ModSummary -> m a) -> a
                 -> m a
foldModSummaries f seed =
  modulesInDepOrder >>= foldM f seed


instance Functor Bag where
  fmap = mapBag

instance Foldable Bag where
  foldr = foldrBag
  foldl = foldlBag

expectJust :: String -> Maybe a -> a
expectJust _ (Just a) = a
expectJust msg Nothing = 
    dieHard $ "Just x expected.\n  grep for \"" ++ msg ++ "\""

unqualifiedForModule :: TypecheckedMod m => m -> ScionM PrintUnqualified
unqualifiedForModule tcm = do
  fromMaybe alwaysQualify `fmap` mkPrintUnqualifiedForModule (moduleInfo tcm)
