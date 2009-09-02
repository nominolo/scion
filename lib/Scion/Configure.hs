{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
-- |
-- Module      : Scion.Configure
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@googlemail.com
-- Stability   : experimental
-- Portability : portable
--
module Scion.Configure where

import Scion.Types
import Scion.Session


import System.Directory
import System.FilePath
import Control.Monad
import Control.Exception ( IOException )

------------------------------------------------------------------------------

