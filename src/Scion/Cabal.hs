{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables, CPP, PatternGuards #-}
-- | Stuff related to working on top of Cabal.  E.g., configuring a
-- project.
--
-- Some functions are in the 'Worker' monad and can therefore be only
-- run on a worker.  This mainly includes functions that may take a
-- while to run.  Other functions are parameterised over the monad and
-- can therefore be run where wanted.
--
--
module Scion.Cabal where

import Scion.Types.Core
import Scion.Types.Session
import Scion.Types.Worker

import           Data.Maybe ( isJust )
import           Data.Typeable ( Typeable )
import           Control.Monad ( when )
import           Distribution.PackageDescription.Parse
import           Distribution.Simple.Build ( initialBuildSteps )
import           Distribution.Simple.Configure
import qualified Distribution.PackageDescription as PD
import qualified Distribution.PackageDescription.Parse as PD
import qualified Distribution.PackageDescription.Configuration as PD
import           Distribution.Simple.Program
import           Distribution.Simple.Setup ( defaultConfigFlags,
                                             ConfigFlags(..), Flag(..) )
import qualified Distribution.Verbosity as V ( normal, deafening, silent )
import           GHC.Paths ( ghc, ghc_pkg )
import           System.Directory
import           System.Exit ( ExitCode(..) )
import           System.FilePath ( dropFileName, takeBaseName )

#if __GLASGOW_HASKELL__ >= 702
import           Distribution.Simple.LocalBuildInfo hiding ( Component, libdir )
#else
import           Distribution.Simple.LocalBuildInfo hiding ( libdir )
import           Distribution.Simple.PreProcess ( knownSuffixHandlers )
#endif

-- | Something went wrong inside Cabal.
data CabalException = CabalException String
  deriving (Typeable)

instance Show CabalException where
  show (CabalException msg) = "CabalException: " ++ msg

instance Exception CabalException

-- | Set up a Cabal component, (re-)configuring it if necessary.
--
-- Checks whether an existing configuration result exists on disk and
-- configures the project if not.  Similarly, if the existing config
-- is outdated the project is reconfigured.
--
-- Configuration is roughly equivalent to calling "./Setup configure"
-- on the command line.  The difference is that this makes sure to use
-- the same version of Cabal and the GHC API that Scion was built
-- against.  This is important to avoid compatibility problems.
--
configureCabalProject :: SessionConfig -> FilePath
                      -> Worker LocalBuildInfo
configureCabalProject conf@CabalConfig{} build_dir = do
  cabal_exists <- io $ doesFileExist cabal_file
  when (not cabal_exists) $
    io $ throwIO $ CabalException $
      ".cabal file does not exist: " ++ cabal_file
  let setup_config = localBuildInfoFile build_dir
  conf'd <- io $ doesFileExist setup_config
  if not conf'd
   then do
     message verbose $ "Configuring for first time: " ++ cabal_file
     do_configure
   else do
     -- check whether setup_config is up to date
     cabal_time <- io $ getModificationTime cabal_file
     conf_time <- io $ getModificationTime setup_config
     if cabal_time >= conf_time
      then do
        message verbose $ "Reconfiguring because .cabal file is newer: "
                            ++ cabal_file
        do_configure
      else do
        mb_lbi <- io $ maybeGetPersistBuildConfig build_dir
        case mb_lbi of
          Nothing -> do
            message verbose $ "Reconfiguring because Cabal says so: "
                                ++ cabal_file
            do_configure
          Just lbi ->
            return lbi

 where
   cabal_file = sc_cabalFile conf

   do_configure =
     ghandle (\(e :: IOError) ->
                 io $ throwIO $
                   CabalException $ "Failed to configure" ++ show e) $ do
       gen_pkg_descr <- io $ readPackageDescription V.normal cabal_file
       -- TODO: The following only works for build-type simple.  We
       -- should support non-standard Setup.hs as well.

       -- Make sure we configure with the same version of GHC
       let prog_conf =
             userSpecifyPaths [("ghc", ghc), ("ghc-pkg", ghc_pkg)]
               defaultProgramConfiguration
       let config_flags =
             (defaultConfigFlags prog_conf)
               { configDistPref = Flag build_dir
               , configVerbosity = Flag V.deafening
               , configUserInstall = Flag True
               -- TODO: parse flags properly
               }
       let root_dir = dropFileName cabal_file
       io $ do
         setCurrentDirectory root_dir
         lbi <- configure (gen_pkg_descr, (Nothing, []))
                          config_flags
         writePersistBuildConfig build_dir lbi
         initialBuildSteps build_dir (localPkgDescr lbi) lbi V.normal
#if __GLASGOW_HASKELL__ < 702
                           knownSuffixHandlers
#endif
         return lbi
configureCabalProject _ _ = fail "configureCabalProject: invalid config type"

availableComponents :: PD.PackageDescription -> [Component]
availableComponents pd =
  (if isJust (PD.library pd) then [Library] else []) ++
      [ Executable (PD.exeName e)
      | e <- PD.executables pd ]

-- | List all possible components of the @.cabal@ given file.
--
-- Some components might not be available depending on the way the
-- program is configured.
fileComponents :: (ExceptionMonad m, MonadIO m) =>
                  FilePath -> m [Component]
fileComponents cabal_file = do
  ghandle (\(_ :: ExitCode) ->
                io $ throwIO $ CabalException $ "Cannot open Cabal file: "
                     ++ cabal_file) $ do
    gpd <- io $ PD.readPackageDescription V.silent cabal_file
    return (availableComponents (PD.flattenPackageDescription gpd))

-- | List all possible default session configs from a given @.cabal@ file.
cabalSessionConfigs :: (ExceptionMonad m, MonadIO m) => FilePath
                    -> m [SessionConfig]
cabalSessionConfigs cabal_file = do
  comps <- fileComponents cabal_file
  return (map (componentToSessionConfig cabal_file) comps)

-- | Create the default configuration for a Cabal file and component.
componentToSessionConfig :: FilePath -> Component -> SessionConfig
componentToSessionConfig cabal_file comp =
     CabalConfig{ sc_name = nameFromComponent comp
                , sc_cabalFile = cabal_file
                , sc_component = comp
                , sc_configFlags = []
                , sc_buildDir = Nothing
                }
 where
   library_name = takeBaseName cabal_file

   nameFromComponent Library = library_name
   nameFromComponent (Executable exe_name) =
     library_name ++ ":" ++ exe_name
