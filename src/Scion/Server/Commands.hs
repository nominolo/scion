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

import Control.Monad
import Data.Foldable as F
import Data.IORef
import Data.Monoid
import Text.ParserCombinators.ReadP
import Numeric   ( showInt )

import qualified Distribution.PackageDescription as PD
import Distribution.Text ( display )

------------------------------------------------------------------------------

allCommands :: [Command]
allCommands = 
    [ cmdConnectionInfo
    , cmdOpenCabalProject
    , cmdLoadComponent ]

------------------------------------------------------------------------------

-- | Used by the client to initialise the connection.
cmdConnectionInfo :: Command
cmdConnectionInfo = Command (string "connection-info" >> return c)
  where
    c = do let pid = 0
           return $ parens (showString ":version" <+> showInt scionVersion <+>
                            showString ":pid" <+> showInt pid)
                  $ ""

cmdOpenCabalProject :: Command
cmdOpenCabalProject =
    Command (string "open-cabal-project" >> sp >> getString >>= return . cmd)
  where
    cmd path = do
        openCabalProject path
        (show . display . PD.package) `fmap` currentCabalPackage

cmdLoadComponent :: Command
cmdLoadComponent =
    Command $ do
      string "load-component" >> sp
      comp <- choice 
                [ string "library" >> return Library
                , inParens $ 
                    string "executable" >> liftM Executable (getString)]
      return (cmd comp)
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
            return $ "(:ok " ++ show (length (toList warns)) ++ ")"
        Failed ->
            return $ "(:error " ++
              show (length (toList errs)) ++ " " ++
              show (length (toList warns)) ++ ")"

    logWarnErr ref err = do
      let errs = case err of
                   Nothing -> mempty
                   Just exc -> srcErrorMessages exc
      warns <- getWarnings
      clearWarnings
      liftIO $ modifyIORef ref $ 
                 \(warns', errs') -> ( warns `mappend` warns'
                                     , errs `mappend` errs')
