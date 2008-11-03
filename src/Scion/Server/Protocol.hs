{-# LANGUAGE ExistentialQuantification #-}
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
import Data.Char ( isHexDigit, digitToInt, isDigit )
import Numeric   ( showInt )
import Control.Monad ( liftM2 )

------------------------------------------------------------------------------

-- TODO: Make these a typeclass?

data Request
  = Rex (IO String) Int -- Remote EXecute
  | RQuit

data Response
  = RReturn String Int
  | RUnknown
  deriving Show

------------------------------------------------------------------------------

-- * Parsing Requests

parseRequest :: [Command] -> String -> Maybe Request
parseRequest cmds msg =
    case readP_to_S (messageParser cmds) msg of
      [(m, "")] -> Just m
      []         -> Nothing
      _         -> error "Ambiguous grammar for message.  This is a bug."

-- | At the moment messages are in a very simple Lisp-style format.  This
--   should also be easy to parse (and generate) for non-lisp clients.
messageParser :: [Command] -> ReadP Request
messageParser cmds =
  inParens $ choice 
    [ string ":quit" >> sp >> return RQuit
    , string ":emacs-rex" >> 
      do c <- choice (map getCommand cmds)
         i <- getInt
         return (Rex c i)
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

getInt :: ReadP Int
getInt = munch1 isDigit >>= return . read

decodeEscapes :: String -> String
decodeEscapes = id -- XXX

-- | One or more spaces.
sp :: ReadP ()
sp = skipMany (char ' ')


------------------------------------------------------------------------------

-- * Writing Responses

showResponse :: Response -> String
showResponse r = shows' r "\n"
  where
    shows' (RReturn f i) = 
        parens (showString ":return" <+> showString f <+> showInt i)
    shows' RUnknown    = showString ":unknown"
--     shows' (RError e)  = parens (showString "error" <+> putString e)
--     shows' (RString s) = putString s
--     shows' (RConnInfo v pid) =
--         parens (showString ":version" <+> showInt v <+>
--                 showString ":pid" <+> showInt pid)

parens :: ShowS -> ShowS
parens p = showChar '(' . p . showChar ')'

putString :: String -> ShowS
putString s = showString (show s)

infixr 1 <+>
(<+>) :: ShowS -> ShowS -> ShowS
l <+> r = l . showChar ' ' . r

--class ToLisp a where toLisp :: a -> ShowS

data Command = Command {
    getCommand :: ReadP (IO String)
  }