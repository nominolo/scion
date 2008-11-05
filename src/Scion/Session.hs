-- |
-- Module      : Scion.Session
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Utilities to manipulate the session state.
--
module Scion.Session where

import GHC
import Scion.Types

import Control.Monad
import Data.List        ( intercalate )
import System.Directory ( setCurrentDirectory )

import Distribution.Simple.LocalBuildInfo hiding ( libdir )
import Distribution.Simple.Configure
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.ModuleName ( components )
import qualified Distribution.PackageDescription as PD

setWorkingDir :: FilePath -> ScionM ()
setWorkingDir home = do
  liftIO $ setCurrentDirectory home
  workingDirectoryChanged

openCabalProject :: FilePath -> ScionM ()
openCabalProject dist_dir = do
  -- XXX: check that working dir contains a .cabal file
  lbi <- liftIO $ getPersistBuildConfig dist_dir
  -- XXX: do something with old lbi before updating?
  modifySessionState $ \st -> st { localBuildInfo = Just lbi }

getLocalBuildInfo :: ScionM LocalBuildInfo
getLocalBuildInfo =
  gets localBuildInfo >>= \mb_lbi ->
  case mb_lbi of
    Nothing -> error "call openCabalProject before loadCabalProject"
    Just lbi -> return lbi

data CabalComponent = Library | Executable String

setDynFlagsFromCabal :: CabalComponent -> ScionM [PackageId]  -- XXX: return modules?
setDynFlagsFromCabal component = do
  lbi <- getLocalBuildInfo
  dflags <- getSessionDynFlags
  let pd = localPkgDescr lbi
  bi <- case component of
         Library
          | Just lib <- PD.library pd -> return (PD.libBuildInfo lib)
          | otherwise -> error "No library in package"
         Executable n ->
          case [ exe | exe <- PD.executables pd, PD.exeName exe == n ] of
           [ exe ] -> return (PD.buildInfo exe)
           [] -> error $ "Executable " ++ n ++ " not found in package"
           _ -> error $ "Multiple executables, named \"" ++ n ++ 
                        "\" found.  This is weird..."
  let odir = buildDir lbi
  let flags = ghcOptions lbi bi odir
  addCmdLineFlags flags

setTargetsFromCabal :: CabalComponent -> ScionM ()
setTargetsFromCabal Library = do
  lbi <- getLocalBuildInfo
  let lib = case PD.library (localPkgDescr lbi) of
              Just l -> l
              Nothing -> error "No library in package."
  let modnames = PD.libModules (localPkgDescr lbi)
  let cabal_mod_to_string m =
        intercalate "." (components m)
  let modname_to_target name =
        Target { targetId = TargetModule (mkModuleName
                                          (cabal_mod_to_string name))
               , targetAllowObjCode = True
               , targetContents = Nothing }
  setTargets (map modname_to_target modnames)
setTargetsFromCabal (Executable n) = do
  error "unimplemented"

addCmdLineFlags :: [String] -> ScionM [PackageId]
addCmdLineFlags flags = do
  dflags <- getSessionDynFlags
  (dflags', unknown, warnings) <- parseDynamicFlags dflags (map noLoc flags)
  unless (null unknown) $
    liftIO $ putStrLn $ "Unrecognised flags:\n" ++ show (map unLoc unknown)
  liftIO $ mapM_ putStrLn $ map unLoc warnings
  setSessionDynFlags dflags'
