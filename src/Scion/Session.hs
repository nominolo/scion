{-# LANGUAGE PatternGuards, DeriveDataTypeable #-}
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
import Data.Data
import Data.List        ( intercalate )
import System.Directory ( setCurrentDirectory )
import Control.Exception

import Distribution.ModuleName ( components )
import Distribution.Simple.Configure
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo hiding ( libdir )
import qualified Distribution.PackageDescription as PD

------------------------------------------------------------------------------

-- TODO: have some kind of project description file, that allows us to
-- reconfigure a project when needed.

------------------------------------------------------------------------------

-- | Sets the current working directory and notifies GHC about the change.
--
-- TODO: do we want to adjust certain flags automatically?
setWorkingDir :: FilePath -> ScionM ()
setWorkingDir home = do
  liftIO $ setCurrentDirectory home
  workingDirectoryChanged

data CannotOpenCabalProject = CannotOpenCabalProject String
     deriving (Show, Typeable)
instance Exception CannotOpenCabalProject

-- | Try to open a configured Cabal project with the given dist/ directory.
--
-- Throws:
--
--  * 'CannotOpenCabalProject' if an error occurs (e.g., not configured
--    project or configured with incompatible cabal version).
--
openCabalProject :: FilePath -> ScionM ()
openCabalProject dist_dir = do
  -- XXX: check that working dir contains a .cabal file
  mb_lbi <- liftIO $ maybeGetPersistBuildConfig dist_dir
  case mb_lbi of
    Nothing -> 
        liftIO $ throwIO $ CannotOpenCabalProject "no reason known" -- XXX
    Just lbi -> do
        -- XXX: do something with old lbi before updating?
        modifySessionState $ \st -> st { localBuildInfo = Just lbi }

data NoCurrentCabalProject = NoCurrentCabalProject deriving (Show, Typeable)
instance Exception NoCurrentCabalProject

getLocalBuildInfo :: ScionM LocalBuildInfo
getLocalBuildInfo =
  gets localBuildInfo >>= \mb_lbi ->
  case mb_lbi of
    Nothing -> liftIO $ throwIO NoCurrentCabalProject
      --error "call openCabalProject before loadCabalProject"
    Just lbi -> return lbi

data CabalComponent = Library | Executable String deriving (Show, Typeable)

data ComponentDoesNotExist = ComponentDoesNotExist CabalComponent
     deriving (Show, Typeable)
instance Exception ComponentDoesNotExist

noLibError :: ScionM a
noLibError = liftIO $ throwIO $ ComponentDoesNotExist Library

noExeError :: String -> ScionM a
noExeError = liftIO . throwIO . ComponentDoesNotExist . Executable 

-- | Set GHC's dynamic flags for the given component of the current Cabal
-- project (see 'openCabalProject').
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
--  * 'ComponentDoesNotExist' if the current Cabal project does not contain
--    the specified component.
--
setDynFlagsFromCabal :: 
       CabalComponent 
    -> ScionM [PackageId]
       -- ^ List of packages that need to be loaded.  This corresponds to the
       -- build-depends of the loaded component.
       --
       -- TODO: do something with this depending on Scion mode?
setDynFlagsFromCabal component = do
  lbi <- getLocalBuildInfo
  dflags <- getSessionDynFlags
  let pd = localPkgDescr lbi
  bi <- case component of
         Library
          | Just lib <- PD.library pd -> return (PD.libBuildInfo lib)
          | otherwise -> noLibError
         Executable n ->
          case [ exe | exe <- PD.executables pd, PD.exeName exe == n ] of
           [ exe ] -> return (PD.buildInfo exe)
           [] -> noExeError n
           _ -> error $ "Multiple executables, named \"" ++ n ++ 
                        "\" found.  This is weird..."
  let odir = buildDir lbi
  let flags = ghcOptions lbi bi odir
  addCmdLineFlags flags

-- | Set the targets for a 'GHC.load' command from the meta data of the
--   current Cabal project.
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
--  * 'ComponentDoesNotExist' if the current Cabal project does not contain
--    the specified component.
--
setTargetsFromCabal :: CabalComponent -> ScionM ()
setTargetsFromCabal Library = do
  lbi <- getLocalBuildInfo
  lib <- case PD.library (localPkgDescr lbi) of
           Just l -> return l
           Nothing -> noLibError
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

-- | Parses the list of 'Strings' as command line arguments and sets the
-- 'DynFlags' accordingly.
--
-- XXX: currently dies on parse failure.
addCmdLineFlags :: [String] -> ScionM [PackageId]
addCmdLineFlags flags = do
  dflags <- getSessionDynFlags
  (dflags', unknown, warnings) <- parseDynamicFlags dflags (map noLoc flags)
  unless (null unknown) $
    liftIO $ putStrLn $ "Unrecognised flags:\n" ++ show (map unLoc unknown)
  liftIO $ mapM_ putStrLn $ map unLoc warnings
  setSessionDynFlags dflags'

-- | Return the (configured) package description of the current Cabal project.
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
currentCabalPackage :: ScionM PD.PackageDescription
currentCabalPackage = do
  lbi <- getLocalBuildInfo
  return (localPkgDescr lbi)

-- | List all components in the current cabal project.
--
-- This can be used to present the user a list of possible items to load.
-- 
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
availableComponents :: ScionM [CabalComponent]
availableComponents = do
  lbi <- getLocalBuildInfo
  let pd = localPkgDescr lbi
  return $ (case PD.library pd of
              Just _ -> [Library]
              _ -> []) ++
           [ Executable n 
                 | PD.Executable {PD.exeName = n} <- PD.executables pd ]
