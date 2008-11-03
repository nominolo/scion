{-# LANGUAGE BangPatterns, DeriveDataTypeable, ScopedTypeVariables, TypeFamilies #-}
module Scion.Server.Emacs where

import Scion.Server.Protocol

import MonadUtils

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

import Control.Exception

data SocketClosed = SocketClosed deriving (Show, Typeable)
instance Exception SocketClosed

runServer :: MonadIO m => m ()
runServer =
    liftIO $
    withSocketsDo $ do
      print $ "starting up server..."
      sock <- listenOn (PortNumber 4005)
      print $ "listing on port 4005"
      loop sock
  where
    loop sock = do
      log 4 "accepting"
      (sock', addr) <- accept sock
      log 4 "starting to serve"
      more <- loop2 sock'
      log 4 "done serving"
      sClose sock'
      log 4 "socket closed"
      if more then loop sock
              else return ()

    -- returns False if server should be shut down,
    -- returns True if socket was closed
    loop2 sock = 
      handle (\(e :: SocketClosed) -> return True) $ do
        r <- getRequest sock
        --log 2 $ "got request: " ++ show r
        case r of
          Nothing -> sendResponse sock RUnknown >> loop2 sock
          Just req 
            | RQuit <- req -> return False
            | otherwise -> 
                handleRequest req >>= sendResponse sock >> loop2 sock

    sendResponse sock r = do
      let payload = S.pack (showResponse r)
      let hdr = mkHeader (S.length payload)
      send sock (S.pack hdr)
      send sock payload
      return ()

myrecv sock 0 = return S.empty
myrecv sock len =
    let handler e | isEOFError e = return S.empty
                  | otherwise = ioError e
    in System.IO.Error.catch (recv sock len) handler

-- | A message is a sequence of bytes, prefixed by the message length encoded
-- as a 6 character hexadecimal number.
getRequest :: Socket -> IO (Maybe Request)
getRequest sock = do
    len_as_hex <- S.unpack `fmap` myrecv sock 6
    len <- case len_as_hex of
             [_,_,_,_,_,_] -> 
                 case readP_to_S parseHex len_as_hex of
                   [(n, "")] -> return n
                   _ -> 
                     error "Could not parse message header."
             _ -> throwIO SocketClosed
    payload <- myrecv sock len
    return $ parseRequest allCommands (S.unpack payload)

parseHex :: ReadP Int
parseHex = munch1 isHexDigit >>= return . go 0
  where
    go !r [] = r
    go !r (c:cs) = go (r `shiftL` 4 + digitToInt c) cs

handleRequest :: Request -> IO Response
handleRequest (Rex r i) = do answer <- r
                             return (RReturn answer i)
--handleRequest _ = return ROk

{-
blockSize :: Int
blockSize = 4 * 1024

chunksToString :: [S.ByteString] -> String
chunksToString = L.unpack . L.fromChunks

handleMessage :: [S.ByteString] -> IO ()
handleMessage chunks =
    let str = chunksToString chunks in
    return ()
-}
mkHeader :: Int -> String
mkHeader len = reverse . take 6 $ reverse (showHex len "") ++ repeat '0'

log :: MonadIO m => Int -> String -> m ()
log _ s = liftIO $ putStrLn s

scionVersion :: Int
scionVersion = 1

connInfo :: Command
connInfo = Command (string "connection-info" >> return c)
  where
    c = do let pid = 0
           return $ parens (showString ":version" <+> showInt scionVersion <+>
                            showString ":pid" <+> showInt pid)
                  $ ""

allCommands :: [Command]
allCommands = [ connInfo ]