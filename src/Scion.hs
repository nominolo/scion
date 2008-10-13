module Scion where

import Scion.Types

import GHC
import GHC.Paths ( libdir )
import MonadUtils ( liftIO, MonadIO )
import System.Directory ( setCurrentDirectory )

import Distribution.Simple.LocalBuildInfo hiding ( libdir )
import Distribution.Simple.Configure

--import Control.Exception

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

openCabalProject :: FilePath -> ScionM ()
openCabalProject dist_dir = do
  lbi <- io $ getPersistBuildConfig dist_dir
  -- XXX: do something with old lbi before updating?
  modifySessionState $ \st -> st { localBuildInfo = Just lbi }
{-
loadCabalProject :: ScionM ()  -- XXX: return modules?
loadCabalProject = do
  lbi <- getLocalBuildInfo
  dflags <- getSessionDynFlags
  (dflags', unknown, warnings) <- parseDynamicFlags dflags lbi
-}
getLocalBuildInfo :: ScionM LocalBuildInfo
getLocalBuildInfo =
  gets localBuildInfo >>= \mb_lbi ->
  case mb_lbi of
    Nothing -> error "call openCabalProject before loadCabalProject"
    Just lbi -> return lbi
