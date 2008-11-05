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
import Scion.Session
import Scion.Server.Protocol

import Text.ParserCombinators.ReadP
import Numeric   ( showInt )

import qualified Distribution.PackageDescription as PD
import Distribution.Text ( display )

------------------------------------------------------------------------------

allCommands :: [Command]
allCommands = 
    [ cmdConnectionInfo
    , cmdOpenCabalProject ]

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
