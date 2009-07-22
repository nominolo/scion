{-# LANGUAGE ScopedTypeVariables, CPP #-}
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
-- Imports
import Prelude hiding ( mod )
import GHC hiding ( flags, load )
import HscTypes ( srcErrorMessages, SourceError, isBootSummary )
import Exception

import Scion.Types
import Scion.Types.Notes
import Scion.Utils
import Scion.Inspect.DefinitionSite

import qualified Data.MultiSet as MS
import Control.Monad
import Data.Data
import Data.IORef
import Data.List        ( intercalate )
import Data.Maybe       ( isJust )
import Data.Monoid
import Data.Time.Clock  ( getCurrentTime, diffUTCTime )
import System.Directory ( setCurrentDirectory, getCurrentDirectory,
                          doesFileExist, getDirectoryContents )
import System.FilePath  ( (</>), isRelative, makeRelative, normalise, 
                          dropFileName, takeDirectory )
import Control.Exception
import System.Exit ( ExitCode(..) )

import qualified Distribution.ModuleName as PD ( ModuleName, components )
import Distribution.Simple.Configure
import Distribution.Simple.GHC ( ghcOptions )
import Distribution.Simple.LocalBuildInfo hiding ( libdir )
import Distribution.Simple.Build ( initialBuildSteps )
import Distribution.Simple.PreProcess ( knownSuffixHandlers )
import qualified Distribution.Verbosity as V
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PD
import qualified Distribution.PackageDescription.Configuration as PD

------------------------------------------------------------------------------

-- TODO: have some kind of project description file, that allows us to
-- reconfigure a project when needed.

------------------------------------------------------------------------------

-- * Exception Types

data CannotOpenCabalProject = CannotOpenCabalProject String
     deriving (Show, Typeable)
instance Exception CannotOpenCabalProject where
  toException  = scionToException
  fromException = scionFromException

data NoCurrentCabalProject = NoCurrentCabalProject deriving (Show, Typeable)
instance Exception NoCurrentCabalProject where
  toException  = scionToException
  fromException = scionFromException

data ComponentDoesNotExist = ComponentDoesNotExist Component
     deriving (Show, Typeable)
instance Exception ComponentDoesNotExist where
  toException  = scionToException
  fromException = scionFromException


-- * Setting Session Parameters


initialScionDynFlags :: DynFlags -> DynFlags
initialScionDynFlags dflags =
  dflags {
      -- GHC 6.10.1 has a bug in that it doesn't properly keep track of which
      -- modules were compiled in HscNothing mode.  To avoid this, we use
      -- HscInterpreted.  Unfortunately, that means we cannot use Scion with
      -- projects that use unboxed tuples, as those are not supported by the
      -- byte code compiler.
#ifdef RECOMPILE_BUG_FIXED
      hscTarget = HscNothing  -- by default, don't modify anything
    , ghcLink   = NoLink      -- just to be sure
#else
      hscTarget = HscInterpreted
    , ghcLink   = LinkInMemory
#endif
    }

-- | Reset the state of the session to a defined default state.
--
-- Due to some bugs in GHC this isn't completely possible.  For example, GHC
-- retains instance declarations which can lead to problems when you load a
-- new module which defines a different instance.  (You'll get a conflicting
-- instance error, which can only be resolved by re-starting GHC.)
resetSessionState :: ScionM ()
resetSessionState = do
   unload

   dflags0 <- gets initialDynFlags
   -- TODO: do something with result from setSessionDynFlags?
   setSessionDynFlags (initialScionDynFlags dflags0)
   return ()

-- | Sets the current working directory and notifies GHC about the change.
--
-- TODO: do we want to adjust certain flags automatically?
setWorkingDir :: FilePath -> ScionM ()
setWorkingDir home = do
  cwd <- liftIO $ getCurrentDirectory
  message deafening $ "Setting working directory: " ++ home ++ " (old: " ++ cwd ++ ")"
  liftIO $ setCurrentDirectory home
  cwd' <- liftIO $ getCurrentDirectory -- to avoid normalisation issues
  when (cwd /= cwd') $ do
    message deafening $ "(Working directory changed.)"
    workingDirectoryChanged

------------------------------------------------------------------------
-- * Cabal Projects

-- | Try to open a Cabal project.  The project must already be configured
-- using the same version of Cabal that Scion was build against.
--
-- Use 'configureCabalProject' to automatically configure a project (if it
-- hasn't been already.)
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

-- | Return path to the .cabal file of the current Cabal package.
--
-- This is useful to identify the project when communicating with Scion from
-- foreign code, because this does not require serialising the local build
-- info.
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project or the
--    current project has no .cabal file.
--
currentCabalFile :: ScionM FilePath
currentCabalFile = do
  lbi <- getLocalBuildInfo
  case pkgDescrFile lbi of
    Just f -> return f
    Nothing -> liftIO $ throwIO $ NoCurrentCabalProject

-- | Return all components of the specified Cabal file.
--
-- Throws:
--
--  * 'CannotOpenCabalProject' if an error occurs (e.g., .cabal file does
--    not exist or could not be parsed.).
--
cabalProjectComponents :: FilePath -- ^ The .cabal file
                       -> ScionM [Component]
cabalProjectComponents cabal_file = do
   ghandle (\(_ :: ExitCode) ->
                liftIO $ throwIO $ CannotOpenCabalProject cabal_file) $ do
     gpd <- liftIO $ PD.readPackageDescription V.silent cabal_file 
     let pd = PD.flattenPackageDescription gpd
     return $
       (if isJust (PD.library pd) then [Library] else []) ++
       [ Executable (PD.exeName e) | e <- PD.executables pd ]


cabalConfigurations :: FilePath -- ^ The .cabal file
                            -> ScionM [CabalConfiguration]
cabalConfigurations cabal = liftIO $ do
  let dir = takeDirectory cabal
  list <- filterM (doesFileExist . (</> "setup-config")) =<< getDirectoryContents dir
  return $ map CabalConfiguration list

-- | Run the steps that Cabal would call before building.
--
preprocessPackage :: FilePath
                  -> ScionM ()
preprocessPackage dist_dir = do
  lbi <- getLocalBuildInfo
  let pd = localPkgDescr lbi
  liftIO $ initialBuildSteps dist_dir pd lbi V.normal knownSuffixHandlers
  return ()

-- | Return the current 'LocalBuildInfo'.
--
-- The 'LocalBuildInfo' is the result of configuring a Cabal project,
-- therefore requires that we have a current Cabal project.
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
getLocalBuildInfo :: ScionM LocalBuildInfo
getLocalBuildInfo =
  gets localBuildInfo >>= \mb_lbi ->
  case mb_lbi of
    Nothing -> liftIO $ throwIO NoCurrentCabalProject
      --error "call openCabalProject before loadCabalProject"
    Just lbi -> return lbi

-- | Root directory of the current Cabal project.
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
projectRootDir :: ScionM FilePath
projectRootDir = do
   -- _ <- getLocalBuildInfo -- ensure we have a current project
   -- TODO: error handling
   liftIO $ getCurrentDirectory

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
setComponentDynFlags :: 
       Component 
    -> ScionM [PackageId]
       -- ^ List of packages that need to be loaded.  This corresponds to the
       -- build-depends of the loaded component.
       --
       -- TODO: do something with this depending on Scion mode?
setComponentDynFlags (File _) = 
   -- The DynFlags within the file are set automatically.
   return []
setComponentDynFlags component = do
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
   component_build_info _ _ =
       dieHard "component_build_info: impossible case"

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
setComponentTargets :: Component -> ScionM ()
setComponentTargets Library = do
  pd <- currentCabalPackage
  unless (isJust (PD.library pd))
    noLibError
  let modnames = PD.libModules pd
  setTargets (map cabalModuleNameToTarget modnames)
setComponentTargets (Executable n) = do
  pd <- currentCabalPackage
  let ex0 = filter ((n==) . PD.exeName) (PD.executables pd)
  case ex0 of
    [] -> noExeError n
    (_:_:_) -> error $ "Multiple executables with name: " ++ n
    [exe] -> do
      proj_root <- cabalProjectRoot
      let others = PD.otherModules (PD.buildInfo exe)
      let main_mods = [ proj_root </> search_path </> PD.modulePath exe 
                          | search_path <- PD.hsSourceDirs (PD.buildInfo exe)]
      (main_mod:_) <- filterM (liftIO . doesFileExist) main_mods
      let targets = Target (TargetFile main_mod Nothing) True Nothing :
                    map cabalModuleNameToTarget others
      setTargets targets
      return ()
setComponentTargets (File f) = do
  setTargets [ Target (TargetFile f Nothing)
                      True
                      Nothing ]
    
cabalModuleNameToTarget :: PD.ModuleName -> Target
cabalModuleNameToTarget name =
   Target { targetId = TargetModule (mkModuleName
                                     (cabal_mod_to_string name))
          , targetAllowObjCode = True
          , targetContents = Nothing }
  where
    cabal_mod_to_string m =
        intercalate "." (PD.components m)

-- | Load the specified component from the current Cabal project.
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
--  * 'ComponentDoesNotExist' if the current Cabal project does not contain
--    the specified component.
--
loadComponent :: Component
              -> ScionM CompilationResult
                 -- ^ The compilation result.
loadComponent comp = do
   -- TODO: group warnings by file
   resetSessionState
   setActiveComponent comp
   maybe_set_working_dir comp
   -- Need to set DynFlags first, so that the search paths are set up
   -- correctly before looking for the targets.
   setComponentDynFlags comp
   setComponentTargets comp
   rslt <- load LoadAllTargets
   mg <- getModuleGraph
   base_dir <- projectRootDir
   db <- moduleGraphDefSiteDB base_dir mg
   liftIO $ evaluate db
   modifySessionState $ \s -> s { lastCompResult = rslt
                                , defSiteDB = db }
   return rslt
  where
    maybe_set_working_dir (File f) = do
      wd <- liftIO $ getCurrentDirectory
      let dir = normalise $ wd </> dropFileName f
      setWorkingDir dir
    maybe_set_working_dir _ = do
      dir <- cabalProjectRoot
      setWorkingDir dir
        
cabalProjectRoot :: ScionM FilePath
cabalProjectRoot = do
  lbi <- getLocalBuildInfo
  let mb_pkg_dir = dropFileName `fmap` pkgDescrFile lbi
  case mb_pkg_dir of
    Just dir -> return dir
    Nothing -> liftIO $ getCurrentDirectory

-- | Make the specified component the active one.  Sets the DynFlags to
--  those specified for the given component.  Unloads the possible
--
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
--  * 'ComponentDoesNotExist' if the current Cabal project does not contain
--    the specified component.
--
setActiveComponent :: Component -> ScionM ()
setActiveComponent comp = do
   curr_comp <- gets activeComponent
   when (needs_unloading curr_comp)
     unload
   setComponentDynFlags comp
   modifySessionState (\sess -> sess { activeComponent = Just comp })
  where
   needs_unloading (Just c) | c /= comp = True
   needs_unloading _ = False

-- | Return the currently active component.
getActiveComponent :: ScionM (Maybe Component)
getActiveComponent = gets activeComponent

-- ** Internal Utilities
noLibError :: ScionM a
noLibError = liftIO $ throwIO $ ComponentDoesNotExist Library

noExeError :: String -> ScionM a
noExeError = liftIO . throwIO . ComponentDoesNotExist . Executable

------------------------------------------------------------------------
-- * Compilation

-- | Wrapper for 'GHC.load'.
load :: LoadHowMuch -> ScionM CompilationResult
load how_much = do
   start_time <- liftIO $ getCurrentTime
   ref <- liftIO $ newIORef (mempty, mempty)
   res <- loadWithLogger (logWarnErr ref) how_much
            `gcatch` (\(e :: SourceError) -> handle_error ref e)
   end_time <- liftIO $ getCurrentTime
   let time_diff = diffUTCTime end_time start_time
   (warns, errs) <- liftIO $ readIORef ref
   base_dir <- projectRootDir
   let notes = ghcMessagesToNotes base_dir (warns, errs)
   let comp_rslt = case res of
                     Succeeded -> CompilationResult True notes time_diff
                     Failed -> CompilationResult False notes time_diff
   -- TODO: We need to somehow find out which modules were recompiled so we
   -- only update the part that we have new information for.
   modifySessionState $ \s -> s { lastCompResult = comp_rslt }
   return comp_rslt
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

-- | Unload whatever is currently loaded.
unload :: ScionM ()
unload = do
   setTargets []
   load LoadAllTargets
   modifySessionState $ \st -> st { lastCompResult = mempty
                                  , defSiteDB = mempty }
   return ()

-- | Parses the list of 'Strings' as command line arguments and sets the
-- 'DynFlags' accordingly.
--
-- Does not set the flags if a parse error occurs.  XXX: There's currently
-- no way to find out if there was an error from inside the program.
addCmdLineFlags :: [String] -> ScionM [PackageId]
addCmdLineFlags flags = do
  message deafening $ "Setting Flags: " ++ show flags
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

-- | List all components in the current cabal project.
--
-- This can be used to present the user a list of possible items to load.
-- 
-- Throws:
--
--  * 'NoCurrentCabalProject' if there is no current Cabal project.
--
availableComponents :: ScionM [Component]
availableComponents = do
  pd <- currentCabalPackage
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

------------------------------------------------------------------------------
-- * Background Typechecking

-- | Takes an absolute path to a file and attempts to typecheck it.
--
-- This performs the following steps:
--
--   1. Check whether the file is actually part of the current project.  It's
--      also currently not possible to typecheck a .hs-boot file using this
--      function.  We simply bail out if these conditions are not met.
--
--   2. Make sure that all dependencies of the module are up to date.
--
--   3. Parse, typecheck, desugar and load the module.  The last step is
--      necessary so that we can we don't have to recompile in the case that
--      we switch to another module.
--
--   4. If the previous step was successful, cache the results in the session
--      for use by source code inspection utilities.  Some of the above steps
--      are skipped if we know that they are not necessary.
--
backgroundTypecheckFile :: 
       FilePath 
    -> ScionM (Bool, CompilationResult)
       -- ^ First element is @False@ <=> step 1 above failed.
backgroundTypecheckFile fname =
   ifM (not `fmap` isRelativeToProjectRoot fname)
     (return (False, mempty))
     prepareContext
  where
   prepareContext = do
     message verbose $ "Preparing context for " ++ fname
     -- if it's the focused module, we know that the context is right
     mb_focusmod <- gets focusedModule
     case mb_focusmod of
       Just ms | Just f <- ml_hs_file (ms_location ms), f == fname -> 
          backgroundTypecheckFile' mempty

       _otherwise -> do
          mb_modsum <- filePathToProjectModule fname
          case mb_modsum of
            Nothing -> do
              message verbose "Could not find file in module graph."
              return (False, mempty)
            Just modsum -> do
              (_, rslt) <- setContextForBGTC modsum
              if compilationSucceeded rslt
               then backgroundTypecheckFile' rslt
               else return (True, rslt)

   backgroundTypecheckFile' comp_rslt = do
      message verbose $ "Background type checking: " ++ fname
      clearWarnings
      start_time <- liftIO $ getCurrentTime
      modsum <- preprocessModule

      let finish_up tc_res errs = do
              base_dir <- projectRootDir
              warns <- getWarnings
              clearWarnings
              let notes = ghcMessagesToNotes base_dir (warns, errs)
              end_time <- liftIO $ getCurrentTime
              let ok = isJust tc_res
              let res = CompilationResult ok notes
                                          (diffUTCTime end_time start_time)
              let abs_fname = mkAbsFilePath base_dir fname
              full_comp_rslt <- removeMessagesForFile abs_fname =<< gets lastCompResult
              let comp_rslt' =  full_comp_rslt `mappend` comp_rslt `mappend` res

              modifySessionState (\s -> s { bgTcCache = tc_res
                                          , lastCompResult = comp_rslt' })

              return (True, comp_rslt')

      ghandle (\(e :: SourceError) -> finish_up Nothing (srcErrorMessages e)) $
        do
          -- TODO: measure time and stop after a phase if it takes too long?
          parsed_mod <- parseModule modsum
          tcd_mod <- typecheckModule parsed_mod
          ds_mod <- desugarModule tcd_mod
          loadModule ds_mod -- ensure it's in the HPT
          finish_up (Just (Typechecked tcd_mod)) mempty

   preprocessModule = do
     depanal [] True
     -- reload-calculate the ModSummary because it contains the cached
     -- preprocessed source code
     mb_modsum <- filePathToProjectModule fname
     case mb_modsum of
       Nothing -> error "Huh? No modsummary after preprocessing?"
       Just ms -> return ms

       
-- | Return whether the filepath refers to a file inside the current project
--   root.  Return 'False' if there is no current project.
isRelativeToProjectRoot :: FilePath -> ScionM Bool
isRelativeToProjectRoot fname = do
   root_dir <- projectRootDir
   return (isRelative (makeRelative root_dir fname))
  `gcatch` \(_ :: NoCurrentCabalProject) -> return False


filePathToProjectModule :: FilePath -> ScionM (Maybe ModSummary)
filePathToProjectModule fname = do
   -- We use the current module graph, don't bother to do a new depanal
   -- if it's empty then we have no current component, hence no BgTcCache.
   --
   -- We check for both relative and absolute filenames because we don't seem
   -- to have any guarantee from GHC what the filenames will look like.
   -- TODO: not sure what happens for names like "../foo"
   root_dir <- projectRootDir
   let rel_fname = normalise (makeRelative root_dir fname)
   mod_graph <- getModuleGraph
   case [ m | m <- mod_graph
            , not (isBootSummary m)
            , Just src <- [ml_hs_file (ms_location m)]
            , src == fname || src == rel_fname || src == "." </> rel_fname ]
    of [ m ] -> do return (Just m)
       _     -> do message verbose $ "No module found for " ++ fname
                   return Nothing  -- ambiguous or not present
  `gcatch` \(_ :: NoCurrentCabalProject) -> return Nothing

isPartOfProject :: FilePath -> ScionM Bool
isPartOfProject fname = fmap isJust (filePathToProjectModule fname)


-- | Ensure that all dependencies of the module are already loaded.
--
-- Sets 'focusedModule' if it was successful.
setContextForBGTC :: ModSummary -> ScionM (Maybe ModuleName, CompilationResult)
setContextForBGTC modsum = do
   message deafening $ "Setting context for: " ++
                       moduleNameString (moduleName (ms_mod modsum))
   let mod_name = ms_mod_name modsum
   start_time <- liftIO $ getCurrentTime
   r <- load (LoadDependenciesOf mod_name) 
           `gcatch` \(e :: SourceError) -> 
               srcErrToCompilationResult start_time e
   modifySessionState $ \sess ->
       sess { focusedModule = if compilationSucceeded r
                               then Just modsum
                               else Nothing
            }
   return (Nothing, r)
  where
    srcErrToCompilationResult start_time err = do
       end_time <- liftIO $ getCurrentTime
       warns <- getWarnings
       clearWarnings
       base_dir <- projectRootDir
       let notes = ghcMessagesToNotes base_dir (warns, srcErrorMessages err)
       return (CompilationResult False notes
                                 (diffUTCTime end_time start_time))
  
-- | Return the 'ModSummary' that refers to the source file.
--
-- Assumes that there is exactly one such 'ModSummary'.
-- 
modSummaryForFile :: FilePath -> ModuleGraph -> ModSummary
modSummaryForFile fname mod_graph =
    case [ m | m <- mod_graph
         , Just src <- [ml_hs_file (ms_location m)]
         , src == fname ]
    of [ m ] -> m
       []    -> dieHard $ "modSummaryForFile: No ModSummary found for " ++ fname
       _     -> dieHard $ "modSummaryForFile: Too many ModSummaries found for "
                          ++ fname


removeMessagesForFile :: AbsFilePath -> CompilationResult -> ScionM CompilationResult
removeMessagesForFile fname res = return res'
  where
    notes = compilationNotes res
    res' = res { compilationNotes = notes' }
    notes' = MS.filter f notes
    f note
      | isValidLoc l, FileSrc fn <- locSource l = fname /= fn
      | otherwise = True
      where l = noteLoc note

-- Local Variables:
-- indent-tabs-mode: nil
-- End:
