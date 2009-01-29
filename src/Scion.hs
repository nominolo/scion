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
  , module Scion.Configure
  ) where

import Scion.Types
import Scion.Session
import Scion.Configure
import Scion.Utils

import GHC
import GHC.Paths ( libdir )

-- | Run the 'ScionM' monad.
runScion :: ScionM a -> IO a
runScion m = do
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    r <- liftIO $ mkSessionState dflags
    setSessionDynFlags (initialScionDynFlags dflags)
    unScionM m r
