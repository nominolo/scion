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
import Scion.Session ( load, resetSessionState )

import GHC hiding ( load )
import Exception
import Data.Typeable
import Outputable

import System.Directory
import System.FilePath
import Control.Monad

data ConfigException = ConfigException deriving (Show, Typeable)
instance Exception ConfigException

configCabalProjectWithArgs :: FilePath -> [String] -> ScionM Bool
configCabalProjectWithArgs cabal_file args =
   ghandle (\(_ :: ConfigException) -> return False) $ do
    ensureFileExists
    let dir = dropFileName cabal_file
    setup <- findSetup dir
    liftIO $ putStrLn $ "Using setup file: " ++ setup
    mainfun <- compileMain setup
    
    return True
  where
    ensureFileExists = do
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
