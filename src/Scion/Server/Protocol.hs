{-# LANGUAGE ExistentialQuantification, TypeSynonymInstances, PatternGuards #-}
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

import Prelude hiding ( span )

import Scion.Types

import ErrUtils ( WarnMsg, ErrMsg(..) )
import Outputable ( showSDoc, showSDocForUser, ppr )
import SrcLoc   ( SrcSpan, isGoodSrcSpan, srcSpanFile, noSrcSpan,
                  srcSpanStartLine, srcSpanStartCol,
                  srcSpanEndLine, srcSpanEndCol )

import Data.Char ( isDigit, isSpace )
import Numeric   ( showInt )
import Text.ParserCombinators.ReadP
import qualified Data.Map as M

------------------------------------------------------------------------------

-- Bump this before every release whenever the wire-protocol is changed
-- (extension-only is fine)
-- 
-- Don't forget to also adjust the client code.
scionVersion :: Int
scionVersion = 1

------------------------------------------------------------------------------

class Sexp a where toSexp :: a -> ShowS
instance Sexp String where toSexp s = showString (show s)
instance Sexp Int where toSexp i = showInt i
instance Sexp Integer where toSexp i = showInt i
instance Sexp () where toSexp _ = showString "nil"
instance Sexp Bool where 
    toSexp True = showChar 't'
    toSexp False = showString "nil"

newtype Lst a = Lst [a]
instance Sexp a => Sexp (Lst a) where
  toSexp (Lst xs) = parens (go xs)
    where go [] = id
          go [y] = toSexp y
          go (y:ys) = toSexp y <+> go ys

newtype Keyword = K String deriving (Eq, Ord, Show)
instance Sexp Keyword where
  toSexp (K s) = showChar ':' . showString s

-- if you need to cheat
newtype ExactSexp = ExactSexp ShowS
instance Sexp ExactSexp where
  toSexp (ExactSexp s) = s

instance (Sexp a, Sexp b) => Sexp (M.Map a b) where
  toSexp m = parens (go (M.assocs m))
    where go ((k,v):r) = toSexp k <+> toSexp v <+> go r
          go [] = id

data Diagnostic
  = DiagWarning WarnMsg
  | DiagError   ErrMsg

instance Sexp SrcSpan where
  toSexp span
    | isGoodSrcSpan span = 
        parens (showString ":loc" <+> showString (show (srcSpanFile span))
                   <+> showInt (srcSpanStartLine span)
                   <+> showInt (srcSpanStartCol span)
                   <+> showInt (srcSpanEndLine span)
                   <+> showInt (srcSpanEndCol span))
    | otherwise =
        parens (showString ":no-loc" <+> showString (showSDoc (ppr span)))

instance Sexp Diagnostic where
  toSexp (DiagWarning msg) = toSexp_diag ":warning" msg
  toSexp (DiagError msg)   = toSexp_diag ":error" msg

toSexp_diag :: String -> ErrMsg -> ShowS
toSexp_diag diag_type msg =
    parens $ showString diag_type <+> toSexp span 
               <+> putString (show_msg (errMsgShortDoc msg))
               <+> putString (show_msg (errMsgExtraInfo msg))
  where
    span | (s:_) <- errMsgSpans msg = s
         | otherwise                = noSrcSpan
    unqual = errMsgContext msg
    show_msg = showSDocForUser unqual

------------------------------------------------------------------------------

data Request
  = Rex (ScionM String) Int -- Remote EXecute
  | RQuit

instance Show Request where
  show (Rex _ i) = "Rex <cmd> " ++ show i
  show RQuit = "RQuit"

data Response
  = RReturn String Int
  | RReaderError String String
  deriving Show

data Command = Command {
    getCommand :: ReadP (ScionM String)
  }

------------------------------------------------------------------------------

-- * Parsing Requests

parseRequest :: [Command] -> String -> Maybe Request
parseRequest cmds msg =
    let rs = readP_to_S (messageParser cmds) msg in
    case [ r | (r, "") <- rs ] of
      [m] -> Just m
      []  -> Nothing
      _   -> error "Ambiguous grammar for message.  This is a bug."

-- | At the moment messages are in a very simple Lisp-style format.  This
--   should also be easy to parse (and generate) for non-lisp clients.
messageParser :: [Command] -> ReadP Request
messageParser cmds = do
  r <- inParens $ choice
         [ string ":quit" >> return RQuit
         , do string ":emacs-rex"
              sp
              c <- inParens (choice (map getCommand cmds))
              sp
              i <- getInt
              return (Rex c i)
         ]    
  skipSpaces
  return r

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
sp = skipMany1 (satisfy isSpace)


------------------------------------------------------------------------------

-- * Writing Responses

showResponse :: Response -> String
showResponse r = shows' r "\n"
  where
    shows' (RReturn f i) = 
       parens (showString ":return" <+> 
               parens (showString ":ok" <+> showString f)
               <+> showInt i)
    shows' (RReaderError s t) = 
        parens (showString ":reader-error" <+>
                showString (show s) <+>
                showString (show t))
parens :: ShowS -> ShowS
parens p = showChar '(' . p . showChar ')'

putString :: String -> ShowS
putString s = showString (show s)

infixr 1 <+>
(<+>) :: ShowS -> ShowS -> ShowS
l <+> r = l . showChar ' ' . r
