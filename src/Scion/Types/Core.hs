module Scion.Types.Core where

import MonadUtils ( MonadIO(..) ) -- from GHC

io :: MonadIO m => IO a -> m a
io = liftIO
{-# INLINE io #-}
