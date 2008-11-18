{-# LANGUAGE ScopedTypeVariables, CPP #-}
-- |
-- Module      : Scion.Server.Commands
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Commands provided by the server.
--
module Scion.Server.Commands ( allCommands ) where

import Prelude as P
import Scion.Types
import Scion.Utils()
import Scion.Session
import Scion.Server.Protocol

import GHC
import Exception
import DynFlags ( supportedLanguages, allFlags )
import Outputable ( ppr, showSDoc )

import Control.Monad
import Data.Foldable as F
import Data.List ( nub )
import Text.ParserCombinators.ReadP
import qualified Data.Map as M

import qualified Distribution.PackageDescription as PD
import Distribution.Text ( display )

#ifndef HAVE_PACKAGE_DB_MODULES
import UniqFM ( eltsUFM )
import Packages ( pkgIdMap )
  
import Distribution.InstalledPackageInfo
#endif


------------------------------------------------------------------------------

allCommands :: [Command]
allCommands = 
    [ cmdConnectionInfo
    , cmdOpenCabalProject
    , cmdLoadComponent
    , cmdListSupportedLanguages
    , cmdListSupportedPragmas
    , cmdListSupportedFlags
    , cmdListRdrNamesInScope
    , cmdListExposedModules
    , cmdSetGHCVerbosity
    , cmdBackgroundTypecheckFile
    , cmdForceUnload
    , cmdAddCmdLineFlag
    ]

------------------------------------------------------------------------------

toString :: Sexp s => s -> String
toString s = toSexp s ""

data OkErr a = Ok a | Error String
instance Sexp a => Sexp (OkErr a) where
  toSexp (Ok a) = parens (showString ":ok " . toSexp a)
  toSexp (Error e) = parens (showString ":error " . toSexp e)

-- encode expected errors as proper return values
handleScionException :: ScionM a -> ScionM (OkErr a)
handleScionException m = do
   (m >>= return . Ok)
   `gcatch` \(e :: SomeScionException) -> return (Error (show e))

------------------------------------------------------------------------------

-- | Used by the client to initialise the connection.
cmdConnectionInfo :: Command
cmdConnectionInfo = Command (string "connection-info" >> return (toString `fmap` c))
  where
    c = do let pid = 0
           return $ M.fromList 
              [ (K "version", scionVersion)
              , (K "pid",     pid)
              ]

cmdOpenCabalProject :: Command
cmdOpenCabalProject =
    Command (do string "open-cabal-project" >> sp
                n <- getString
                d <- sp >> getString
                return (toString `fmap` cmd n d))
  where
    cmd path rel_dist = handleScionException $ do
        openCabalProject path rel_dist
        (display . PD.package) `fmap` currentCabalPackage

cmdLoadComponent :: Command
cmdLoadComponent =
    Command $ do
      string "load-component" >> sp
      comp <- choice 
                [ string "library" >> return Library
                , inParens $ 
                    string "executable" >> liftM Executable (getString)]
      return (toString `fmap` cmd comp)
  where
    cmd comp = handleScionException $ do
      loadComponent comp
        
instance Sexp CompilationResult where
  toSexp (CompilationResult succeeded warns errs time) = toSexp $
      ExactSexp $ parens $ 
        showString "compilation-result" <+>
        toSexp succeeded <+>
        toSexp (Lst (map DiagWarning (toList warns))) <+>
        toSexp (Lst (map DiagError (toList errs))) <+>
        toSexp (ExactSexp (showString (show 
                  (fromRational (toRational time) :: Float))))

cmdListSupportedLanguages :: Command
cmdListSupportedLanguages =
    Command $ do
      string "list-supported-languages"
      return (return (toString (Lst supportedLanguages)))

cmdListSupportedPragmas :: Command
cmdListSupportedPragmas =
    Command $ do
      string "list-supported-pragmas"
      return (return (toString (Lst supportedPragmas)))

supportedPragmas :: [String]
supportedPragmas =
    [ "OPTIONS_GHC", "LANGUAGE", "INCLUDE", "WARNING", "DEPRECATED"
    , "INLINE", "NOINLINE", "RULES", "SPECIALIZE", "UNPACK", "SOURCE"
    , "SCC"
    , "LINE" -- XXX: only used by code generators, still include?
    ]

cmdListSupportedFlags :: Command
cmdListSupportedFlags =
    Command $ do
      string "list-supported-flags"
      return (return (toString (Lst (nub allFlags))))

cmdListRdrNamesInScope :: Command
cmdListRdrNamesInScope =
    Command $ do
      string "list-rdr-names-in-scope"
      return $ do
        rdr_names <- getNamesInScope
        return (toString (Lst (map (showSDoc . ppr) rdr_names)))

allExposedModules :: ScionM [ModuleName]
#ifdef HAVE_PACKAGE_DB_MODULES
allExposedModules = map moduleName `fmap` packageDbModules True
#else
-- This implementation requires our Cabal to be the same as GHC's.
allExposedModules = do
   dflags <- getSessionDynFlags
   let pkg_db = pkgIdMap (pkgState dflags)
   return $ P.concat (map exposedModules (filter exposed (eltsUFM pkg_db)))
#endif

cmdListExposedModules :: Command
cmdListExposedModules =
    Command $ do
      string "list-exposed-modules"
      return $ do
        mod_names <- allExposedModules
        return $ toString $ Lst $
          map (showSDoc . ppr) mod_names

cmdSetGHCVerbosity :: Command
cmdSetGHCVerbosity =
    Command $ do
      string "set-ghc-verbosity" >> sp
      lvl <- getInt
      return $ do
        toString `fmap` setGHCVerbosity lvl

cmdBackgroundTypecheckFile :: Command
cmdBackgroundTypecheckFile =
    Command $ do
      string "background-typecheck-file" >> sp
      fname <- getString
      return $ do
        toString `fmap` (handleScionException $ backgroundTypecheckFile fname)

cmdForceUnload :: Command
cmdForceUnload =
    Command $ do
      string "force-unload"
      return $
        toString `fmap` unload

cmdAddCmdLineFlag :: Command
cmdAddCmdLineFlag =
    Command $ do
      string "add-command-line-flag" >> sp
      str <- getString
      return $
        toString `fmap` (addCmdLineFlags [str] >> return ())