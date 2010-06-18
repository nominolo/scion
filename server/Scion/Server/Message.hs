{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable,
             ScopedTypeVariables #-}
module Scion.Server.Message
  ( MsgData(..), mkMap, Message(..), Result(..), decodeKey
  , recvMessage, sendMessage
  , recvMessageFromHandle, sendMessageToHandle
  , recvMessageFromSocket, sendMessageToSocket
  , hRecv
  )
where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString      as S
import qualified Data.ByteString.UTF8 as U
import qualified Data.Text            as T
import Data.Text.Encoding ( decodeUtf8With, encodeUtf8 )
import Data.Text.Encoding.Error ( lenientDecode )
import qualified Data.Map             as M
import qualified Data.Binary.Builder  as B
import Control.Monad ( forM_ )
import Control.Applicative
import Control.Exception ( handle )
import Data.Binary
import Data.Binary.Get ( getWord32le, runGet )
import Data.Binary.Put ( putWord32le, runPut )
import Data.Bits ( (.&.), (.|.), shiftL )
import Data.Char ( ord, chr, digitToInt, isSpace )
import Data.Data ( Data(..), Typeable(..) )
import Data.List ( foldl' )
import Data.Monoid ( Monoid(..) )
import Data.Ratio ( (%) )
import Data.String ( IsString(..) )
import Data.Word ( Word8, Word32 )
import GHC.IO.Handle
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO

-- | A generic, untyped message intended to be send over a wire.
--
-- This is the common data structure used by the Scion server and
-- Scion workers.  Front-ends use a different wire format, which is
-- translated into this structure.
--
data MsgData
  = MsgNull
  | MsgBool {-# UNPACK #-} !Bool
  | MsgInt {-# UNPACK #-} !Integer
  | MsgDouble {-# UNPACK #-} !Double
  | MsgText T.Text
  | MsgList [MsgData]
  | MsgMap (M.Map T.Text MsgData)
  | MsgBinary L.ByteString
  deriving (Eq, Ord, Show, Data, Typeable)

mkMap :: [(T.Text, MsgData)] -> MsgData
mkMap = MsgMap . M.fromList

class Message a where
  toMsg :: a -> MsgData
  fromMsg :: MsgData -> Result a

data Result a = Ok a | Error String

instance Functor Result where
  fmap f (Ok a) = Ok (f a)
  fmap f (Error str) = (Error str)

instance Applicative Result where
  pure = Ok
  (Ok f) <*> (Ok a) = Ok (f a)
  (Error s) <*> _ = Error s
  _ <*> (Error s) = Error s

instance Monad Result where
  return = Ok
  (Ok a) >>= f = f a
  (Error s) >>= _ = Error s
  fail msg = Error msg

decodeKey :: Message a => MsgData -> T.Text -> Result a
decodeKey (MsgMap m) key =
  case M.lookup key m of
    Just msg -> fromMsg msg
    Nothing -> fail $ "lookupKey: Key not found: " ++ show key
decodeKey _ _ = fail "lookupKey: Not a map"

instance Message MsgData where
  toMsg = id
  fromMsg = pure

instance Binary MsgData where
  put MsgNull         = putWord8 1
  put (MsgBool False) = putWord8 2
  put (MsgBool True)  = putWord8 3
  put (MsgInt i)      = putWord8 4 >> put i
  put (MsgDouble d)   = putWord8 5 >> put d
  put (MsgText t) =
    -- this hopefully gets fused/inlined
    putWord8 16 >> put (U.fromString (T.unpack t))
  put (MsgList l) = putWord8 17 >> go l
    where
      go [] = putWord8 0
      go (x:xs) = put x >> go xs
  put (MsgMap m) = do
    putWord8 18
    put (M.size m)
    forM_ (M.toList m) $ \(key, val) -> do
      put (U.fromString (T.unpack key))
      put val

  put (MsgBinary bs) =
    putWord8 19 >> put bs

  get = do
    tag <- getWord8
    get_tagged tag
   where
     get_tagged tag =
       case tag of
         1 -> return $ MsgNull
         2 -> return $ MsgBool False
         3 -> return $ MsgBool True
         4 -> MsgInt <$> get
         5 -> MsgDouble <$> get
         16 -> MsgText . T.pack . U.toString <$> get
         17 -> MsgList <$> get_list
         18 -> MsgMap <$> get_map
         19 -> MsgBinary <$> get

     get_list = do
       tag <- getWord8
       case tag of
         0 -> return []
         _ -> (:) <$> get_tagged tag <*> get_list

     get_map = do
       l <- get :: Get Int
       M.fromAscList <$> (sequence $ replicate l $
         (,) <$> (T.pack . U.toString <$> get) <*> get)


fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

instance Num MsgData where
  fromInteger = MsgInt  -- this is what we're interested in
  (MsgInt n) + (MsgInt m) = MsgInt (n + m)
  (MsgDouble n) + (MsgInt m) = MsgDouble (n + fi m)
  (MsgInt n) + (MsgDouble m) = MsgDouble (fi n + m)
  _ + _ = MsgNull
  (MsgInt n) * (MsgInt m) = MsgInt (n * m)
  (MsgDouble n) * (MsgInt m) = MsgDouble (n * fi m)
  (MsgInt n) * (MsgDouble m) = MsgDouble (fi n * m)
  _ * _ = MsgNull
  abs (MsgInt n) = MsgInt (abs n)
  abs (MsgDouble n) = MsgDouble (abs n)
  abs _ = MsgNull
  signum (MsgInt n) = MsgInt (signum n)
  signum (MsgDouble n) = MsgDouble (signum n)
  signum _ = MsgNull

instance IsString MsgData where
  fromString = MsgText . T.pack

------------------------------------------------------------------------

instance Message Bool where
  toMsg b = MsgBool b
  fromMsg (MsgBool b)  = pure b
  fromMsg (MsgList []) = pure False
  fromMsg MsgNull      = pure False
  fromMsg (MsgInt n)   = pure (n /= 0)
  fromMsg (MsgText t)
    | T.null t = pure False
  fromMsg (MsgMap m)
    | M.null m = pure False
  fromMsg _ = fail "Not a Bool"

instance Message () where
  toMsg _ = MsgNull
  fromMsg (MsgNull) = pure ()
  fromMsg (MsgBool False) = pure ()
  fromMsg (MsgInt 0) = pure ()
  fromMsg (MsgText t)
    | T.null t = pure ()
  fromMsg (MsgMap m)
    | M.null m = pure ()
  fromMsg _ = fail "Not unit"

instance Message Integer where
  toMsg i = MsgInt i
  fromMsg MsgNull = pure 0
  fromMsg (MsgBool True) = pure 1
  fromMsg (MsgBool False) = pure 0
  fromMsg (MsgInt i) = pure i
  fromMsg (MsgDouble d) = pure (round d)
  fromMsg _ = fail "Not an integer"

instance Message Int where
  toMsg i = MsgInt (fromIntegral i)
  fromMsg m = toInt <$> fromMsg m
    where toInt = fromIntegral :: Integer -> Int

instance Message T.Text where
  toMsg t = MsgText t
  fromMsg MsgNull = pure T.empty
  fromMsg (MsgText t) = pure t
  fromMsg _ = fail "Not text"

instance Message a => Message [a] where
  toMsg ls = MsgList $ map toMsg ls
  fromMsg MsgNull = pure []
  fromMsg (MsgList l) = mapM fromMsg l

-- TODO: Is this instance OK?  It does not allow nested Maybe types.
-- It's meant to be more in line with the JSON/BSON data model.  I.e.,
--
--     Maybe X = nullable X
--
instance Message a => Message (Maybe a) where
  toMsg Nothing = MsgNull
  toMsg (Just x) = toMsg x
  fromMsg MsgNull = pure Nothing -- Masks 'Just ()' and 'Just Nothing'
                                 -- as 'Nothing'
  fromMsg x = Just <$> fromMsg x

instance (Message a, Message b) => Message (Either a b) where
  toMsg (Right b) = mkMap [("ok", toMsg b)]
  toMsg (Left a) = mkMap [("error", toMsg a)]
  fromMsg m
    | Ok a <- decodeKey m "error" = pure (Left a)
    | Ok b <- decodeKey m "ok" = pure (Right b)
    | otherwise = fail "Either"

------------------------------------------------------------------------
-- * Sending and Receiving Messages
------------------------------------------------------------------------

die :: String -> a
die msg = error $ "ScionWorker: " ++ msg

chunkSize :: Int
chunkSize = 4096

-- | Read a message from the Handle 
recvMessageFromHandle :: Handle -> IO MsgData
recvMessageFromHandle inp =
  recvMessage (hRecv inp chunkSize)

hRecv :: Handle -> Int -> IO S.ByteString
hRecv h size = do
  handle (\(e :: IOError) -> do
            --putStrLn $ "IO Error: " ++ show e
            return S.empty) $ do
    -- Note: hWaitForInput tries to decode its input, so we must make
    -- sure the handle is in binary mode.
    hWaitForInput h (-1)
    S.hGetNonBlocking h size

recvMessageFromSocket :: Socket -> IO MsgData
recvMessageFromSocket sock =
  recvMessage (recv sock chunkSize)

-- | Reads and decodes a message using the given function to retrieve
-- a chunk.
--
-- A message is prefixed with its length (encoded as a 32 bit little
-- endian value).  This is needed because "Data.Binary" does not
-- provide an iteratee interface (it cannot parse partial input).
recvMessage :: IO S.ByteString -- ^ Returns a chunk
             -> IO MsgData
recvMessage get_chunk = do
  chunk <- get_chunk
  if S.length chunk < 4 then return MsgNull else do
   let (len_enc, rest) = S.splitAt 4 chunk
   let len = runGet getWord32le (L.fromChunks [len_enc])
   --putStrLn $ "Msg length: " ++ show len
   decode . L.fromChunks <$> get_chunks len rest
 where
   i2w = fromIntegral
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
   not_enough_input =
     die "not enough input"


sendMessageToHandle :: Handle -> MsgData -> IO ()
sendMessageToHandle out message = do
  sendMessage (L.hPut out) message
  hFlush out

sendMessageToSocket :: Socket -> MsgData -> IO ()
sendMessageToSocket sock message =
  sendMessage (sendMany sock . L.toChunks) message

sendMessage :: (L.ByteString -> IO ()) -> MsgData -> IO ()
sendMessage send_str message = do
  let output = encode message
      len_enc = runPut (putWord32le (fromIntegral (L.length output)))
  --putStrLn $ "Sending: " ++ show (L.unpack len_enc ++ L.unpack output)
  send_str (L.append len_enc output)

------------------------------------------------------------------------
