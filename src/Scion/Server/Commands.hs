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
import Scion.Server.Protocol

import Text.ParserCombinators.ReadP
import Numeric   ( showInt )

------------------------------------------------------------------------------

allCommands :: [Command]
allCommands = [ connInfo ]

------------------------------------------------------------------------------

-- | Used by the client to initialise the connection.
connInfo :: Command
connInfo = Command (string "connection-info" >> return c)
  where
    c = do let pid = 0
           return $ parens (showString ":version" <+> showInt scionVersion <+>
                            showString ":pid" <+> showInt pid)
                  $ ""

