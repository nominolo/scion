module Scion where

import Scion.Types

import GHC
import GHC.Paths ( libdir )
import MonadUtils ( liftIO, MonadIO )
import System.Directory ( setCurrentDirectory )

io :: MonadIO m => IO a -> m a
io = liftIO

runScion :: ScionM a -> IO a
runScion m = do
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags
    r <- io mkSessionState
    unScionM m r

setWorkingDir :: FilePath -> ScionM ()
setWorkingDir home = do
  io $ setCurrentDirectory home
  workingDirectoryChanged

