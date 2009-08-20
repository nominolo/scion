{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
-- |
-- Module      : Scion.Configure
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@googlemail.com
-- Stability   : experimental
-- Portability : portable
--
module Scion.Configure where

import Scion.Types
import Scion.Session

import GHC hiding ( load )
import DynFlags   ( dopt_set )
import GHC.Paths  ( ghc, ghc_pkg )
import Exception
import Data.Typeable
import Outputable

import System.Directory
import System.FilePath
import System.IO ( openTempFile, hPutStr, hClose )
import Control.Monad
import Control.Exception ( IOException )

import Distribution.Simple.Configure
import Distribution.PackageDescription.Parse ( readPackageDescription )
import qualified Distribution.Verbosity as V ( normal, deafening )
import Distribution.Simple.Program ( defaultProgramConfiguration, userSpecifyPaths )
import Distribution.Simple.Setup ( defaultConfigFlags, ConfigFlags(..), Flag(..) )

------------------------------------------------------------------------------

-- | Open or configure a Cabal project using the Cabal library.
--
-- Tries to open an existing Cabal project or configures it if opening
-- failed.
--
-- Throws:
--
--  * 'CannotOpenCabalProject' if configuration failed.
--
openOrConfigureCabalProject :: 
     FilePath -- ^ The project root.  (Where the .cabal file resides)
  -> FilePath -- ^ dist dir, i.e., directory where to put generated files.
  -> [String] -- ^ command line arguments to "configure".
  -> ScionM ()
openOrConfigureCabalProject root_dir dist_dir extra_args =
   openCabalProject root_dir dist_dir
  `gcatch` (\(_ :: CannotOpenCabalProject) -> 
                configureCabalProject root_dir dist_dir extra_args)

-- | Configure a Cabal project using the Cabal library.
--
-- This is roughly equivalent to calling "./Setup configure" on the command
-- line.  The difference is that this makes sure to use the same version of
-- Cabal and the GHC API that Scion was built against.  This is important to
-- avoid compatibility problems.
--
-- If configuration succeeded, sets it as the current project.
--
-- TODO: Figure out a way to report more helpful error messages.
--
-- Throws:
--
--  * 'CannotOpenCabalProject' if configuration failed.
--
configureCabalProject :: 
     FilePath -- ^ The project root.  (Where the .cabal file resides)
  -> FilePath -- ^ dist dir, i.e., directory where to put generated files.
  -> [String] -- ^ command line arguments to "configure". [XXX: currently
              -- ignored!]
  -> ScionM ()
configureCabalProject root_dir dist_dir _extra_args = do
   cabal_file <- find_cabal_file
   gen_pkg_descr <- liftIO $ readPackageDescription V.normal cabal_file
   let prog_conf =
         userSpecifyPaths [("ghc", ghc), ("ghc-pkg", ghc_pkg)]
           defaultProgramConfiguration
   let config_flags = 
         (defaultConfigFlags prog_conf)
           { configDistPref = Flag dist_dir
           , configVerbosity = Flag V.deafening
           , configUserInstall = Flag True
           -- TODO: parse flags properly
           }
   setWorkingDir root_dir
   ghandle (\(_ :: IOError) ->
               liftIO $ throwIO $ 
                CannotOpenCabalProject "Failed to configure") $ do
     lbi <- liftIO $ configure (Left gen_pkg_descr, (Nothing, [])) config_flags
     liftIO $ writePersistBuildConfig dist_dir lbi
     openCabalProject root_dir dist_dir

 where
   find_cabal_file = do
      fs <- liftIO $ getDirectoryContents root_dir
      case [ f | f <- fs, takeExtension f == ".cabal" ] of
        [f] -> return $ root_dir </> f
        [] -> liftIO $ throwIO $ CannotOpenCabalProject "no .cabal file"
        _ -> liftIO $ throwIO $ CannotOpenCabalProject "Too many .cabal files"

-- | Something went wrong during "cabal configure".
--
-- TODO: Add more detail.
data ConfigException = ConfigException deriving (Show, Typeable)
instance Exception ConfigException

-- | Do the equivalent of @runghc Setup.hs <args>@ using the GHC API.
--
-- Instead of "runghc", this function uses the GHC API so that the correct
-- version of GHC and package database is used.
--
-- TODO: Return exception or error message in failure case.
cabalSetupWithArgs ::
     FilePath -- ^ Path to .cabal file.  TODO: ATM, we only need the
              -- directory
  -> [String] -- ^ Command line arguments.
  -> ScionM Bool
cabalSetupWithArgs cabal_file args =
   ghandle (\(_ :: ConfigException) -> return False) $ do
    ensureCabalFileExists
    let dir = dropFileName cabal_file
    (setup, delete_when_done) <- findSetup dir
    liftIO $ putStrLn $ "Using setup file: " ++ setup
    _mainfun <- compileMain setup
    when (delete_when_done) $
      liftIO (removeFile setup)
    return True
  where
    ensureCabalFileExists = do
      ok <- liftIO (doesFileExist cabal_file)
      unless ok (liftIO $ throwIO ConfigException)

    findSetup dir = do
      let candidates = map ((dir </> "Setup.")++) ["lhs", "hs"]
      existing <- mapM (liftIO . doesFileExist) candidates
      case [ f | (f,ok) <- zip candidates existing, ok ] of
        f:_ -> return (f, False)
        [] -> liftIO $ do
          ghandle (\(_ :: IOException) -> throwIO $ ConfigException) $ do
            tmp_dir <- getTemporaryDirectory
            (fp, hdl) <- openTempFile tmp_dir "Setup.hs"
            hPutStr hdl (unlines default_cabal_setup)
            hClose hdl
            return (fp, True)
                     
    default_cabal_setup =
      ["#!/usr/bin/env runhaskell",
       "import Distribution.Simple",
       "main :: IO ()",
       "main = defaultMain"]

    compileMain file = do
      resetSessionState

      dflags <- getSessionDynFlags
      setSessionDynFlags $
        dopt_set (dflags { hscTarget = HscInterpreted
                         , ghcLink   = LinkInMemory
                         })
                 Opt_ForceRecomp -- to avoid picking up Setup.{hi,o}

      t <- guessTarget file Nothing
      liftIO $ putStrLn $ "target: " ++ (showSDoc $ ppr t)
      setTargets [t]
      load LoadAllTargets
      m <- findModule (mkModuleName "Main") Nothing
      env <- findModule (mkModuleName "System.Environment") Nothing
      GHC.setContext [m] [env]
      mainfun <- runStmt ("System.Environment.withArgs "
                                ++ show args
                                ++ "(main)")
                         RunToCompletion
      return mainfun
