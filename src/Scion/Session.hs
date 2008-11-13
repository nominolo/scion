{-# LANGUAGE ScopedTypeVariables #-}
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

import GHC hiding ( flags, load )
import HscTypes ( srcErrorMessages,SourceError )
import Exception
import ErrUtils ( WarningMessages, ErrorMessages )

import Scion.Types
import Scion.Utils()

import Control.Monad
import Data.Data
import Data.IORef
import Data.List        ( intercalate )
import Data.Maybe       ( isJust )
import Data.Monoid
import System.Directory ( setCurrentDirectory )
import System.FilePath  ( (</>) )
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

-- ** Exception Types

data CannotOpenCabalProject = CannotOpenCabalProject String
     deriving (Show, Typeable)
instance Exception CannotOpenCabalProject where
  toException  = scionToException
  fromException = scionFromException

data NoCurrentCabalProject = NoCurrentCabalProject deriving (Show, Typeable)
instance Exception NoCurrentCabalProject where
  toException  = scionToException
  fromException = scionFromException

data ComponentDoesNotExist = ComponentDoesNotExist CabalComponent
     deriving (Show, Typeable)
instance Exception ComponentDoesNotExist where
  toException  = scionToException
  fromException = scionFromException


-- ** Setting Session Parameters


initialScionDynFlags :: DynFlags -> DynFlags
initialScionDynFlags dflags =
  dflags 
    { hscTarget = HscNothing  -- by default, don't modify anything
    , ghcLink   = NoLink      -- just to be sure
    }

resetSessionState :: ScionM ()
resetSessionState = do
   -- unload
   setTargets []
   load LoadAllTargets
   dflags0 <- gets initialDynFlags
   -- TODO: do something with result from setSessionDynFlags?
   setSessionDynFlags (initialScionDynFlags dflags0)
   return ()

-- ** Other Stuff

-- | Sets the current working directory and notifies GHC about the change.
--
-- TODO: do we want to adjust certain flags automatically?
setWorkingDir :: FilePath -> ScionM ()
setWorkingDir home = do
  liftIO $ setCurrentDirectory home
  workingDirectoryChanged

-- | Try to open a Cabal project.  The project must already be configured
-- using the same version of Cabal that Scion was build against.
--
-- TODO: Allow configuration of the project from inside Scion.
--
-- TODO: Allow other working directories?  Would require translating all the
-- search paths from relative to absolute paths.  Furthermore, what should the
-- output directory be then?
--
-- Throws:
--
--  * 'CannotOpenCabalProject' if an error occurs (e.g., not configured
--    project or configured with incompatible cabal version).
--
openCabalProject :: FilePath  -- ^ Project root directroy
                 -> FilePath  -- ^ Project dist directory (relative)
                 -> ScionM ()
openCabalProject root_dir dist_rel_dir = do
  -- XXX: check that working dir contains a .cabal file
  let dist_dir = root_dir </> dist_rel_dir
  mb_lbi <- liftIO $ maybeGetPersistBuildConfig dist_dir
  case mb_lbi of
    Nothing -> 
        liftIO $ throwIO $ CannotOpenCabalProject "no reason known" -- XXX
    Just lbi -> do
        setWorkingDir root_dir
        resetSessionState
        -- XXX: do something with old lbi before updating?
        modifySessionState $ \st -> st { localBuildInfo = Just lbi }

getLocalBuildInfo :: ScionM LocalBuildInfo
getLocalBuildInfo =
  gets localBuildInfo >>= \mb_lbi ->
  case mb_lbi of
    Nothing -> liftIO $ throwIO NoCurrentCabalProject
      --error "call openCabalProject before loadCabalProject"
    Just lbi -> return lbi

data CabalComponent = Library | Executable String deriving (Show, Typeable)

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
   bi <- component_build_info component (localPkgDescr lbi)
   let odir = buildDir lbi
   let flags = ghcOptions lbi bi odir
   addCmdLineFlags flags
 where
   component_build_info Library pd
       | Just lib <- PD.library pd = return (PD.libBuildInfo lib)
       | otherwise                 = noLibError
   component_build_info (Executable n) pd =
       case [ exe | exe <- PD.executables pd, PD.exeName exe == n ] of
         [ exe ] -> return (PD.buildInfo exe)
         [] -> noExeError n
         _ -> error $ "Multiple executables, named \"" ++ n ++ 
                      "\" found.  This is weird..."


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
  unless (isJust (PD.library (localPkgDescr lbi)))
    noLibError
  let modnames = PD.libModules (localPkgDescr lbi)
  let cabal_mod_to_string m =
        intercalate "." (components m)
  let modname_to_target name =
        Target { targetId = TargetModule (mkModuleName
                                          (cabal_mod_to_string name))
               , targetAllowObjCode = True
               , targetContents = Nothing }
  setTargets (map modname_to_target modnames)
setTargetsFromCabal (Executable _) = do
  error "unimplemented"

type CompilationResult
  = (Either (WarningMessages, ErrorMessages)
            WarningMessages)

-- | Load the specified component from the current Cabal project.
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
--  * 'ComponentDoesNotExist' if the current Cabal project does not contain
--    the specified component.
--
loadComponent :: CabalComponent
              -> ScionM CompilationResult
                 -- ^ @Left (warnings, errors)@ if an error occured.  If
                 -- errors is empty, compilation/loading failed due to @-Werror@.
                 --
                 -- @Right warnings@ if compilation/loading succeeded.
loadComponent comp = do
   -- TODO: group warnings by file
   setDynFlagsFromCabal comp
   setTargetsFromCabal comp
   load LoadAllTargets

-- | Wrapper for 'GHC.load'.
load :: LoadHowMuch -> ScionM CompilationResult
load how_much = do
   ref <- liftIO $ newIORef (mempty, mempty)
   res <- loadWithLogger (logWarnErr ref) how_much
            `gcatch` (\(e :: SourceError) -> handle_error ref e)
   (warns, errs) <- liftIO $ readIORef ref
   case res of
     Succeeded -> return (Right warns)
     Failed -> return (Left (warns, errs))
  where
    logWarnErr ref err = do
      let errs = case err of
                   Nothing -> mempty
                   Just exc -> srcErrorMessages exc
      warns <- getWarnings
      clearWarnings
      add_warn_err ref warns errs

    add_warn_err ref warns errs =
      liftIO $ modifyIORef ref $
                 \(warns', errs') -> ( warns `mappend` warns'
                                     , errs `mappend` errs')

    handle_error ref e = do
       let errs = srcErrorMessages e
       warns <- getWarnings
       add_warn_err ref warns errs
       clearWarnings
       return Failed

-- | Parses the list of 'Strings' as command line arguments and sets the
-- 'DynFlags' accordingly.
--
-- Does not set the flags if a parse error occurs.  XXX: There's currently
-- no way to find out if there was an error from inside the program.
addCmdLineFlags :: [String] -> ScionM [PackageId]
addCmdLineFlags flags = do
  dflags <- getSessionDynFlags
  res <- gtry $ parseDynamicFlags dflags (map noLoc flags)
  case res of
    Left (UsageError msg) -> do
      liftIO $ putStrLn $ "Dynflags parse error: " ++ msg
      return []
    Left e -> liftIO $ throwIO e
    Right (dflags', unknown, warnings) -> do
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

-- | Set the verbosity of the GHC API.
setGHCVerbosity :: Int -> ScionM ()
setGHCVerbosity lvl = do
   dflags <- getSessionDynFlags
   setSessionDynFlags $! dflags { verbosity = lvl }
   return ()

