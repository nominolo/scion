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
import GHC.Paths  ( ghc, ghc_pkg )
import Exception
import Data.Typeable
import Outputable

import System.Directory
import System.FilePath
import Control.Monad

------------------------------------------------------------------------------

configureCabalProject :: FilePath -> FilePath -> [String] -> ScionM ()
configureCabalProject root_dir dist_dir extra_args =
   openCabalProject root_dir dist_dir
  `gcatch` (\(_ :: CannotOpenCabalProject) -> do
     cabal_file <- find_cabal_file
     let dist_dir' = "dist-scion"
     let args = [ "configure"
                , "-v"
                , "--builddir=" ++ dist_dir'
                , "--with-compiler=" ++ ghc
                , "--with-hc-pkg=" ++ ghc_pkg
                ] ++ extra_args
     liftIO $ print args
     setWorkingDir root_dir
     ok <- cabalSetupWithArgs cabal_file args
     if ok then openCabalProject root_dir dist_dir'
           else liftIO $ throwIO $ 
                  CannotOpenCabalProject "Failed to configure")
 where
   find_cabal_file = do
      fs <- liftIO $ getDirectoryContents root_dir
      case [ f | f <- fs, takeExtension f == ".cabal" ] of
        [f] -> return $ root_dir </> f
        [] -> liftIO $ throwIO $ CannotOpenCabalProject "no .cabal file"
        _ -> liftIO $ throwIO $ CannotOpenCabalProject "Too many .cabal files"

data ConfigException = ConfigException deriving (Show, Typeable)
instance Exception ConfigException

cabalSetupWithArgs :: FilePath -> [String] -> ScionM Bool
cabalSetupWithArgs cabal_file args =
   ghandle (\(_ :: ConfigException) -> return False) $ do
    ensureCabalFileExists
    let dir = dropFileName cabal_file
    setup <- findSetup dir
    liftIO $ putStrLn $ "Using setup file: " ++ setup
    _mainfun <- compileMain setup
    
    return True
  where
    ensureCabalFileExists = do
      ok <- liftIO (doesFileExist cabal_file)
      unless ok (liftIO $ throwIO ConfigException)

    findSetup dir = do
      let candidates = map ((dir </> "Setup.")++) ["lhs", "hs"]
      existing <- mapM (liftIO . doesFileExist) candidates
      case [ f | (f,ok) <- zip candidates existing, ok ] of
        [] -> liftIO $ throwIO ConfigException
        f:_ -> return f

    compileMain file = do
      resetSessionState

      dflags <- getSessionDynFlags
      setSessionDynFlags dflags { hscTarget = HscInterpreted
                                , ghcLink   = LinkInMemory
                                }

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
