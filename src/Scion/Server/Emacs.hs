{-# LANGUAGE TypeSynonymInstances, BangPatterns #-}
module Scion.Server.Emacs where

import Scion.Server.Protocol

import MonadUtils

import Numeric ( showHex )
import qualified Data.ByteString.Char8 as S
--import qualified Data.ByteString.Lazy.Char8 as L
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO.Error (catch, isEOFError)
import Text.ParserCombinators.ReadP
import Data.Char ( isHexDigit, digitToInt )
import Data.Bits ( shiftL )

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
      print "accepting"
      (sock', addr) <- accept sock
      print "starting to serve"
      more <- loop2 sock'
      print "done serving"
      sClose sock'
      print "socket closed"
      if more then loop sock
              else return ()

    loop2 sock = do
      r <- getRequest sock
      putStrLn $ "got request: " ++ show r
      case r of
        Nothing -> sendResponse sock RUnknown >> loop2 sock
        Just req 
          | req == Stop -> return False
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
-- as a 3 character hexadecimal number.
getRequest :: Socket -> IO (Maybe Request)
getRequest sock = do
    len_as_hex <- S.unpack `fmap` myrecv sock 3
    len <- case len_as_hex of
             [_,_,_] -> 
                 case readP_to_S parseHex len_as_hex of
                   [(n, "")] -> return n
                   _ -> error "Could not parse message header."
             _ -> error "Length header too short"
    payload <- myrecv sock len
    return $ parseRequest (S.unpack payload)

parseHex :: ReadP Int
parseHex = munch1 isHexDigit >>= return . go 0
  where
    go !r [] = r
    go !r (c:cs) = go (r `shiftL` 4 + digitToInt c) cs

handleRequest :: Request -> IO Response
handleRequest _ = return ROk

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
mkHeader len =
  case showHex len "" of
    s@[_] -> ' ':' ':s
    s@[_,_] -> ' ':s
    s@[_,_,_] -> s
    _ -> error "Message too big"