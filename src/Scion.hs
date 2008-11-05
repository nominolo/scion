{-# LANGUAGE PatternGuards #-}
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
