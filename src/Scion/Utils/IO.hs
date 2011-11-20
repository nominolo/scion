{-# LANGUAGE ScopedTypeVariables #-}
module Scion.Utils.IO where

import Data.Binary
import System.IO
import qualified Data.ByteString      as S
import qualified Data.ByteString.Lazy as L
import Control.Applicative
import Data.Binary.Get ( getWord32le, runGet )
import Data.Binary.Put ( putWord32le, runPut )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Exception ( handle )
import GHC.IO.Handle ( hDuplicate, hDuplicateTo )
import Control.Monad ( when )
import Data.Maybe ( isJust )

-- | Read a message from the Handle 
recvMessageFromHandle :: Binary a => Handle -> IO (Maybe a)
recvMessageFromHandle inp =
  recvMessage (hRecv inp chunkSize)

-- | Read a message from the socket.
recvMessageFromSocket :: Binary a => Socket -> IO (Maybe a)
recvMessageFromSocket sock =
  recvMessage (recv sock chunkSize)

-- | Reads and decodes a message using the given function to retrieve
-- a chunk.
--
-- A message is prefixed with its length (encoded as a 32 bit little
-- endian value).  This is needed because "Data.Binary" does not
-- provide an iteratee interface (it cannot parse partial input).
recvMessage :: Binary a => 
               IO S.ByteString -- ^ Returns a chunk
            -> IO (Maybe a)
recvMessage get_chunk = do
  chunk <- get_chunk
  if S.length chunk < 4 then return Nothing else do
   let (len_enc, rest) = S.splitAt 4 chunk
   let len = runGet getWord32le (L.fromChunks [len_enc])
   --putStrLn $ "Msg length: " ++ show len
   Just . decode . L.fromChunks <$> get_chunks len rest
 where
   i2w = fromIntegral :: Int -> Word32
   get_chunks len chunk
     | S.null chunk =
       die "socket closed while decoding message"
     | len > chunk_len = do
       chunk' <- get_chunk
       (chunk:) <$> get_chunks (len - chunk_len) chunk'
     | len == chunk_len =
       return [chunk]
     | otherwise =
       die "input too long"
    where
      chunk_len = i2w (S.length chunk)

   -- TODO: we might want to retry
   _not_enough_input =
     die "not enough input"

-- | Send a message to the handle.
sendMessageToHandle :: Binary a => Handle -> a -> IO ()
sendMessageToHandle out message = do
  sendMessage (L.hPut out) message
  hFlush out

sendMessageToSocket :: Binary a => Socket -> a -> IO ()
sendMessageToSocket sock message =
  sendMessage (sendMany sock . L.toChunks) message

-- | Sends a message using the given function.  The message is sent
-- using a format readable by 'recvMessage'.
sendMessage :: Binary a => (L.ByteString -> IO ()) -> a -> IO ()
sendMessage send_str message_ = do
  let output = encode message_
      len_enc = runPut (putWord32le (fromIntegral (L.length output)))
  --putStrLn $ "Sending: " ++ show (L.unpack len_enc ++ L.unpack output)
  send_str (L.append len_enc output)

chunkSize :: Int
chunkSize = 4096

-- | Receive a message from the given handle, blocks if no input is
-- available.
hRecv :: Handle
      -> Int -- ^ Maximum size of returned bytestring.
      -> IO S.ByteString
hRecv h size = do
  handle (\(_e :: IOError) -> do
            --putStrLn $ "IO Error: " ++ show e
            return S.empty) $ do
    -- Note: hWaitForInput tries to decode its input, so we must make
    -- sure the handle is in binary mode.
    _ <- hWaitForInput h (-1)
    S.hGetNonBlocking h size

die :: String -> a
die msg = error $ "FATAL: " ++ msg

-- | Get exclusive access to the first handle's resource.
--
-- Subsequent writes to the first handle are redirected to the second
-- handle.  The returned handle is an exclusive handle to the resource
-- initially held by the first handle.
makeExclusive ::
     Handle -- ^ The handle to the resource that we want exclusive
            -- access to.
  -> Handle -- ^ Anything written to the original handle will be
            -- redirected to this one.
  -> IO Handle -- ^ The exclusive handle.
makeExclusive hexcl hredirect = do
  hFlush hexcl
  hFlush hredirect
  hresult <- hDuplicate hexcl
  hDuplicateTo hredirect hexcl
  return hresult

-- | Ensure that the handle is in binary mode.
ensureBinaryMode :: Handle -> IO ()
ensureBinaryMode h = do
  enc <- hGetEncoding h
  when (isJust enc) $
    hSetBinaryMode h True

