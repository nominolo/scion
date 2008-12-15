-- |
-- Module      : Scion.Types.ExtraInstances
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@googlemail.com
-- Stability   : experimental
-- Portability : portable
--
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Scion.Types.ExtraInstances where

import Bag

import Data.Monoid

instance Monoid (Bag a) where
  mempty = emptyBag
  mappend = unionBags
  mconcat = unionManyBags

