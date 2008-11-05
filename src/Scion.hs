-- |
-- Module      : Scion
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Scion is a library on top of the GHC API to provide IDE-like functionality.
--
module Scion 
  ( ScionM
  , liftIO, MonadIO
  , module Scion  -- at least for now
  , module Scion.Session
  , module Scion.Utils
  ) where

import Scion.Types
import Scion.Session
import Scion.Utils

import GHC
import GHC.Paths ( libdir )


runScion :: ScionM a -> IO a
runScion m = do
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags (initialScionDynFlags dflags)
    r <- liftIO mkSessionState
    unScionM m r

initialScionDynFlags :: DynFlags -> DynFlags
initialScionDynFlags dflags =
  dflags 
    { hscTarget = HscNothing  -- by default, don't modify anything
    , ghcLink   = NoLink      -- just to be sure
    }
