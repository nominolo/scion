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
module Scion.Server.Commands where

import Scion.Types
import Scion.Utils
import Scion.Session
import Scion.Server.Protocol

import GHC
import HscTypes ( srcErrorMessages )
import DynFlags ( supportedLanguages )

import Control.Monad
import Data.Foldable as F
import Data.IORef
import Data.Monoid
import Text.ParserCombinators.ReadP
import qualified Data.Map as M

import qualified Distribution.PackageDescription as PD
import Distribution.Text ( display )

------------------------------------------------------------------------------

allCommands :: [Command]
allCommands = 
    [ cmdConnectionInfo
    , cmdOpenCabalProject
    , cmdLoadComponent
    , cmdListSupportedLanguages
    , cmdListSupportedPragmas
    ]

------------------------------------------------------------------------------

toString :: Sexp s => s -> String
toString s = toSexp s ""

data OkErr a = Ok a | Error String
instance Sexp a => Sexp (OkErr a) where
  toSexp (Ok a) = parens (showString ":ok " . toSexp a)
  toSexp (Error e) = parens (showString ":error " . toSexp e)

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
                return (toString `fmap` cmd n))
  where
    cmd path = do
        openCabalProject path
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
    cmd comp = do
      -- TODO: group warnings by file
      ref <- liftIO $ newIORef (mempty, mempty)
      setDynFlagsFromCabal comp
      setTargetsFromCabal comp
      res <- loadWithLogger (logWarnErr ref) LoadAllTargets
      (warns, errs) <- liftIO $ readIORef ref
      case res of
        Succeeded -> 
            return $ ExactSexp $ parens $
                showString ":ok" <+> shows (length (toList warns))
        Failed ->
            return $ ExactSexp $ parens $ 
                showString ":error" <+>
                shows (length (toList errs)) <+>
                shows (length (toList warns))

    logWarnErr ref err = do
      let errs = case err of
                   Nothing -> mempty
                   Just exc -> srcErrorMessages exc
      warns <- getWarnings
      clearWarnings
      liftIO $ modifyIORef ref $ 
                 \(warns', errs') -> ( warns `mappend` warns'
                                     , errs `mappend` errs')

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
    , "LINE" -- XXX: only used by code generators, still include?
    ]