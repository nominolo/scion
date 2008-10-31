-- |
-- Module      : Scion.Server.Protocol
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Server message types and methods for serialising and deserialising them to
-- strings.
--
-- TODO: Document protocol + message format.
--
module Scion.Server.Protocol where

import Text.ParserCombinators.ReadP
import Data.Char ( isHexDigit, digitToInt )

------------------------------------------------------------------------------

-- TODO: Make these a typeclass?

data Request
  = Hello String
  | Stop
  | OpenProject String
  | TypeofId String
  deriving (Eq, Ord, Show, Read)

data Response
  = ROk
  | RUnknown
  | RError String
  | RString String
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------

-- * Parsing Requests

parseRequest :: String -> Maybe Request
parseRequest msg =
    case readP_to_S messageParser msg of
      [(m, "")] -> Just m
      []         -> Nothing
      _         -> error "Ambiguous grammar for message.  This is a bug."

-- | At the moment messages are in a very simple Lisp-style format.  This
--   should also be easy to parse (and generate) for non-lisp clients.
messageParser :: ReadP Request
messageParser =
  inParens $ choice 
    [ string "hello" >> sp >> Hello `fmap` getString
    , string "type-of" >> sp >> TypeofId `fmap` getString
    , string "hasta-la-vista" >> return Stop
    ]
    
inParens :: ReadP a -> ReadP a
inParens = between (char '(') (char ')')

getString :: ReadP String
getString = decodeEscapes `fmap` (char '"' >> munchmunch False)
  where
    munchmunch had_backspace = do 
      c <- get
      if c == '"' && not had_backspace 
        then return []
        else do
          (c:) `fmap` munchmunch (c == '\\')

decodeEscapes :: String -> String
decodeEscapes = id -- XXX

-- | One or more spaces.
sp :: ReadP ()
sp = skipMany (char ' ')


------------------------------------------------------------------------------

-- * Writing Responses

showResponse :: Response -> String
showResponse r = shows' r ""
  where
    shows' ROk         = showString "ok"
    shows' RUnknown    = showString "unknown"
    shows' (RError e)  = parens (showString "error" <+> putString e)
    shows' (RString s) = putString s

parens :: ShowS -> ShowS
parens p = showChar '(' . p . showChar ')'

putString :: String -> ShowS
putString s = showString (show s)

infixr 1 <+>
(<+>) :: ShowS -> ShowS -> ShowS
l <+> r = l . showChar ' ' . r