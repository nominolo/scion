{-# LANGUAGE BangPatterns, DeriveDataTypeable, ScopedTypeVariables,
             TypeFamilies, PatternGuards #-}
-- |
-- Module      : Scion.Server.Emacs
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- An example server that communicates with Emacs.
--
module Scion.Server.Emacs where

import Scion.Types
import Scion.Server.Protocol
import Scion.Server.Commands

import Exception
import MonadUtils

import Control.Exception
import Control.Monad ( liftM )
import Data.Bits ( shiftL )
import Data.Char ( isHexDigit, digitToInt )
import Data.Data ( Typeable )
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Numeric ( showHex, showInt )
import Prelude hiding ( log )
import System.IO.Error (catch, isEOFError)
import Text.ParserCombinators.ReadP
import qualified Data.ByteString.Char8 as S

------------------------------------------------------------------------------

data SocketClosed = SocketClosed deriving (Show, Typeable)
instance Exception SocketClosed

runServer :: ScionM ()
runServer =
    reifyScionM $ \s ->
      withSocketsDo $ do
        log 1 "starting up server..."
        sock <- liftIO $ listenOn (PortNumber 4005)
        log 1 "listing on port 4005"
        reflectScionM (loop sock) s
  where
    loop sock = do
      log 4 "accepting"
      (sock', addr) <- liftIO $ accept sock
      log 4 "starting to serve"
      more <- eventLoop sock'
      log 4 "done serving"
      liftIO $ sClose sock'
      log 4 "socket closed"
      if more then loop sock
              else return ()

eventLoop :: Socket -> ScionM Bool
eventLoop sock = 
    ghandle (\(e :: SocketClosed) -> return True) $ do
      (r, s) <- getRequest sock
      case r of
        Nothing -> do
               log 1 "Could not parse request."
               sendResponse sock (RReaderError s "no parse")
               eventLoop sock
        Just req
          | RQuit <- req -> return False
          | otherwise -> do
             resp <- handleRequest req
             log 4 (show resp)
             sendResponse sock resp
             eventLoop sock
  where
    sendResponse sock r = do
      let payload = S.pack (showResponse r)
      let hdr = mkHeader (S.length payload)
      liftIO $ do 
        send sock (S.pack hdr)
        send sock payload
      return ()

myrecv :: MonadIO m => Socket -> Int -> m S.ByteString
myrecv sock 0 = return S.empty
myrecv sock len =
    let handler e | isEOFError e = return S.empty
                  | otherwise = ioError e
    in liftIO $ System.IO.Error.catch (recv sock len) handler

-- | A message is a sequence of bytes, prefixed by the message length encoded
-- as a 6 character hexadecimal number.
getRequest :: MonadIO m => Socket -> m (Maybe Request, String)
getRequest sock = do
    len_as_hex <- liftM S.unpack (myrecv sock 6)
    len <- case len_as_hex of
             [_,_,_,_,_,_] -> 
                 case readP_to_S parseHex len_as_hex of
                   [(n, "")] -> return n
                   _ -> 
                     error "Could not parse message header."
             _ -> liftIO $ throwIO SocketClosed
    payload <- myrecv sock len
    log 4 (show (len_as_hex, payload))
    let s = (S.unpack payload)
    return $ (parseRequest allCommands s, s)

parseHex :: ReadP Int
parseHex = munch1 isHexDigit >>= return . go 0
  where
    go !r [] = r
    go !r (c:cs) = go (r `shiftL` 4 + digitToInt c) cs

handleRequest :: Request -> ScionM Response
handleRequest (Rex r i) = do answer <- r
                             return (RReturn answer i)

mkHeader :: Int -> String
mkHeader len = reverse . take 6 $ reverse (showHex len "") ++ repeat '0'

log :: MonadIO m => Int -> String -> m ()
log _ s = liftIO $ putStrLn s





