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

import Scion.Utils()
import Scion.Session
import Scion.Server.Protocol

import DynFlags ( supportedLanguages, allFlags )

import Control.Monad
import Data.Foldable as F
import Data.List ( nub )
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
    , cmdListSupportedFlags
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
      r <- loadComponent comp
      case r of
        Left (warns, errs) ->
            return $ ExactSexp $ parens $ 
              showString ":error" <+>
              shows (length (toList errs)) <+>
              shows (length (toList warns))
        Right warns ->
            return $ ExactSexp $ parens $
              showString ":ok" <+> shows (length (toList warns))

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

cmdListSupportedFlags :: Command
cmdListSupportedFlags =
    Command $ do
      string "list-supported-flags"
      return (return (toString (Lst (nub allFlags))))
