{-# LANGUAGE OverloadedStrings, CPP, DeriveDataTypeable,
             ScopedTypeVariables #-}
module Scion.Server.Message
  ( MsgData(..), mkMap, Message(..)
  , parseJson, parseLisp
  , endOfLine, endOfInput, parseLazyBS
  , recvMessage, sendMessage
  , recvMessageFromHandle, sendMessageToHandle
  , recvMessageFromSocket, sendMessageToSocket
  , hRecv
  , encodeJson
#ifndef NDEBUG
  , test_Scion_Server_Message
#endif
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
import Data.Attoparsec
import Data.Attoparsec.Char8 ( endOfLine, skipSpace )
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

#ifndef NDEBUG
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding ( (.&.) )

import Data.String ( fromString )
import Debug.Trace
#endif

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
  fromMsg :: MsgData -> Maybe a

instance Message MsgData where
  toMsg = id
  fromMsg = Just

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
  fromMsg (MsgBool b)  = Just b
  fromMsg (MsgList []) = Just False
  fromMsg MsgNull      = Just False
  fromMsg (MsgInt n)   = Just (n /= 0)
  fromMsg (MsgText t)
    | T.null t = Just False
  fromMsg (MsgMap m)
    | M.null m = Just False
  fromMsg _ = Nothing

instance Message () where
  toMsg _ = MsgNull
  fromMsg (MsgNull) = Just ()
  fromMsg (MsgBool False) = Just ()
  fromMsg (MsgInt 0) = Just ()
  fromMsg (MsgText t)
    | T.null t = Just ()
  fromMsg (MsgMap m)
    | M.null m = Just ()
  fromMsg _ = Nothing

instance Message Integer where
  toMsg i = MsgInt i
  fromMsg MsgNull = Just 0
  fromMsg (MsgBool True) = Just 1
  fromMsg (MsgBool False) = Just 0
  fromMsg (MsgInt i) = Just i
  fromMsg (MsgDouble d) = Just (round d)
  fromMsg _ = Nothing

instance Message T.Text where
  toMsg t = MsgText t
  fromMsg MsgNull = Just T.empty
  fromMsg (MsgText t) = Just t
  fromMsg _ = Nothing

instance Message a => Message [a] where
  toMsg ls = MsgList $ map toMsg ls
  fromMsg MsgNull = Just []
  fromMsg (MsgList l) = mapM fromMsg l
  
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
-- * JSON Wire Format
------------------------------------------------------------------------

-- | Length in Word8s
lenUTF8 :: Word8 -> Int
{-# INLINE lenUTF8 #-}
lenUTF8 w8
  | w8 < 0x80 = 1
  | w8 < 0xe0 = 2
  | w8 < 0xf0 = 3
  | w8 < 0xf8 = 4
  | otherwise = 0

utf8Char :: Parser Char
utf8Char = do
  c1 <- anyWord8
  case lenUTF8 c1 of
    1 -> return $ b2c c1
    2 -> do
      c2 <- anyWord8
      return $ w2c $ ((b2w c1 .&. 0x1f) `shiftL` 6) .|. (b2w c2 .&. 0x3f)
    3 -> do
      ensure 2
      c2 <- anyWord8
      c3 <- anyWord8
      return $ w2c $ ((b2w c1 .&. 0x0f) `shiftL` 12) .|.
                     ((b2w c2 .&. 0x3f) `shiftL` 6) .|.
                     (b2w c3 .&. 0x3f)
    4 -> do
      ensure 3
      c2 <- anyWord8
      c3 <- anyWord8
      c4 <- anyWord8
      return $ w2c $ ((b2w c1 .&. 0x07) `shiftL` 18) .|.
                     ((b2w c2 .&. 0x3f) `shiftL` 12) .|.
                     ((b2w c3 .&. 0x3f) `shiftL` 6) .|.
                     (b2w c4 .&. 0x3f)
    0 -> return replacement_char

char :: Char -> Parser Word8
char c = word8 (fromIntegral $ ord c)

-- | This character is used to mark errors in a UTF8 encoded string.
replacement_char :: Char
replacement_char = '\xfffd'


b2c :: Word8 -> Char
b2c = chr . fromIntegral

c2b :: Char -> Word8
c2b = fromIntegral . ord

b2w :: Word8 -> Word32
b2w = fromIntegral

w2c :: Word32 -> Char
w2c = chr . fromIntegral

token :: Parser a -> Parser a
token p = p <* skipSpace

parseLazyBS :: Parser a -> L.ByteString
            -> Either String a
parseLazyBS p lbs = go (parse p) (L.toChunks lbs)
 where
   go f [] =
     case f S.empty of
       Fail _ _ msg -> Left msg
       Partial f' -> Left "unexpected eof"
       Done c' r  -> Right r
   go f (c:cs) =
     case f c of
       Fail _ _ msg -> Left msg
       Partial f' -> go f' cs
       Done c' r  -> Right r

encodeJson :: MsgData -> B.Builder
encodeJson MsgNull = B.fromByteString "null"
encodeJson (MsgBool False) = B.fromByteString "false"
encodeJson (MsgBool True) = B.fromByteString "true"
encodeJson (MsgInt n) = B.fromByteString (U.fromString (show n))
encodeJson (MsgDouble d) = B.fromByteString (U.fromString (show d))
encodeJson (MsgText t) = encode_json_text t
encodeJson (MsgList []) = B.fromByteString "[]"
encodeJson (MsgList (x:xs)) = 
  B.singleton (c2b '[') `mappend` encodeJson x `mappend` go xs
 where
   go [] = B.singleton (c2b ']')
   go (y:ys) =
     B.singleton (c2b ',') `mappend` encodeJson y `mappend` go ys
encodeJson (MsgMap m)
  | M.null m = B.fromByteString "{}"
  | otherwise =
    B.singleton (c2b '{') `mappend` enc_pair x `mappend` go xs
 where
   (x:xs) = M.toList m
   enc_pair (k, v) = encode_json_text k `mappend`
                     B.singleton (c2b ':') `mappend`
                      encodeJson v
   go [] = B.singleton (c2b '}')
   go (y:ys) =
     B.singleton (c2b ',') `mappend` enc_pair y `mappend` go ys

encode_json_text txt_ =
  B.singleton (c2b '"') `mappend` go txt_ `mappend` B.singleton (c2b '"')
 where
   go txt
     | T.null txt = mempty
     | otherwise =
       let (noquote, rest) = T.breakBy needs_quoting txt in
       B.fromByteString (encodeUtf8 noquote) `mappend`
        (if T.null rest then mempty else
           quote (T.head rest) `mappend` go (T.tail rest))
   needs_quoting c =
     c `elem` ['"', '\\', '/', '\n', '\r', '\t', '\b', '\f']
   quote '"' = B.fromByteString "\\\""
   quote '\\' = B.fromByteString "\\\\"
   quote '/' = B.fromByteString "\\/"
   quote '\n' = B.fromByteString "\\n"
   quote '\r' = B.fromByteString "\\r"
   quote '\t' = B.fromByteString "\\t"
   quote '\b' = B.fromByteString "\\b"
   quote '\f' = B.fromByteString "\\f"

parseJson :: Parser MsgData
parseJson = skipSpace *> (json_val <* skipSpace)
 where
   json_val =
     MsgNull <$ token (string "null") <|>
     MsgBool True <$ token (string "true") <|>
     MsgBool False <$ token (string "false") <|>
     token json_num <|>
     MsgText <$> token json_string <|>
     json_list <|>
     json_obj

   json_list =
     MsgList <$> (token (char '[') *>
                  json_val `sepBy` token (char ',')
                  <* token (char ']'))

   json_obj =
     MsgMap . M.fromList <$>
       (token (char '{') *>
        (((,) <$> token json_string <*> (token (char ':') *> json_val))
          `sepBy` token (char ',')) <*
        token (char '}'))

   json_string = utf8_encoded_string quoted_char
    where
      hex_digit = b2c <$> satisfy (inClass "0-9a-fA-F") <?> "hex digit"
      quoted_char = do
        c <- b2c <$> anyWord8
        case c of
          '"' -> return c
          '\\' -> return c
          '/' -> return c
          'b' -> return '\b'
          'f' -> return '\f'
          'n' -> return '\n'
          'r' -> return '\r'
          't' -> return '\t'
          'u' -> do
            ensure 4
            chr . from_hex <$> count 4 hex_digit
          _ -> return c -- let's not be strict
      from_hex = foldl' (\n r -> n `shiftL` 4 .|. digitToInt r) 0

utf8_encoded_string :: Parser Char -> Parser T.Text
utf8_encoded_string quoted_char = T.concat <$> (char '"' *> go)
 where
   go = do
     -- the following is safe in UTF8 encoding
     chunk <- takeTill (\c -> c == c2b '"' || c == c2b '\\')
        --trace (show chunk) (return ())
     c <- b2c <$> anyWord8
     case c of
       '\\' -> do c <- quoted_char;
                  (\r -> decode chunk `T.snoc` c : r) <$> go
       '"' -> return [decode chunk]

   decode = decodeUtf8With lenientDecode

d2i :: Integral a => Word8 -> a
d2i w8 = fromIntegral $ w8 - c2b '0'

digit :: Integral a => Parser a
digit = d2i <$> satisfy (between '0' '9')

non_zero_int :: Integer -> Parser Integer
non_zero_int acc =
  (do d <- digit
      non_zero_int (acc * 10 + d))
  <|> pure acc

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

between :: Char -> Char -> Word8 -> Bool
between lo hi c = c >= c2b lo && c <= c2b hi

json_num :: Parser MsgData
json_num = do
  inv <- (\x -> -x) <$ char '-' <|> pure id
  int <- 0 <$ char '0' <|>
         (satisfy (between '1' '9') >>= non_zero_int . d2i)
  (frac, frac_len, e) <-
    char '.' *> parse_frac <|>
    (char 'e' <|> char 'E') *> parse_exp 0 0 <|>
    return (0, 0, 0)
  case () of
   _ | frac == 0 && e >= 0 ->
     return $ MsgInt $ if e == 0 then inv int else inv int * 10 ^ e
     | fi frac_len <= e ->
       return $ MsgInt $ inv $ int * 10 ^ e + frac * 10 ^ (e - fi frac_len)
     | frac == 0 && int == 0 ->
       return $ MsgInt 0
     | frac == 0 ->
       return $ MsgDouble $ fromRational $ fi (inv int) * 10 ^^ e
     | otherwise ->
       let r :: Rational
           r = fi (inv int) * 10 ^^ e + fi (inv frac) * 10 ^^ (e - fi frac_len)
       in return $ MsgDouble $ fromRational r
 where
   parse_frac = go 0 0
    where
      go :: Integer -> Int -> Parser (Integer, Int, Integer)
      go acc len =
        (do c <- satisfy (between '0' '9')
            go (acc * 10 + d2i c) (len + 1))
        <|> char 'e' *> parse_exp acc len
        <|> return (acc, len, 0)

   parse_exp frac frac_len =do
     inv <- (\x -> -x) <$ char '-' <|> id <$ char '+' <|> pure id
     go inv =<< digit
    where
      go inv acc = (do d <- digit; go inv (acc * 10 + d)) <|>
                   return (frac, frac_len, inv acc)


parseLisp :: Parser MsgData
parseLisp = skipSpace *> s_expr
 where
   s_expr =
     lisp_list <|> lisp_atom  -- note order
   lisp_list =
     -- Automatically convert lists that alternates text and values
     -- into a map.  This is a bit dodgy but quite useful (as long as
     -- it doesn't cause any problems down the road).
     maybe_as_map <$>
       (token (char '(') *> s_expr `sepBy` sp <* token (char ')'))
   
   maybe_as_map [] = MsgList []
   maybe_as_map lst =
     case as_map M.empty lst of
       Nothing -> MsgList lst
       Just m -> MsgMap m

   as_map :: M.Map T.Text MsgData -> [MsgData]
          -> Maybe (M.Map T.Text MsgData)
   as_map m [] = Just m
   as_map m (MsgText key:val:r) =
     as_map (M.insert key val m) r
   as_map m _ = Nothing

   lisp_atom =
     -- Lisp symbols may start with digits, e.g., `1foo` is valid
     -- symbol, but `1.0` should still be parsed as a number.  So we
     -- try to parse it as a number first and fall back to parsing it
     -- as a symbol if that fails.
     try json_num <|>
     MsgText <$> utf8_encoded_string decode_quoted <|>
     (do sym <- takeWhile1 is_symbol_char
         case () of
           _ | sym == "nil" -> return MsgNull
             | sym == "t" -> return (MsgBool True)
             | S.head sym == c2b ':' ->
               return (MsgText (decode (S.tail sym)))
             | otherwise ->
               return (MsgText (decode sym)))
   decode = decodeUtf8With lenientDecode
   sp = satisfy is_space *> skipWhile is_space
   is_space w8 = let c = b2c w8 in isSpace c
   is_symbol_char =
     not . inClass " '\"\t\r\n()"
   decode_quoted = do
     c <- b2c <$> anyWord8
     case c of
       '"' -> return c
       '\\' -> return c
       '/' -> return c
       'b' -> return '\b'
       'f' -> return '\f'
       'n' -> return '\n'
       'r' -> return '\r'
       't' -> return '\t'
       _ -> return c

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

#ifndef NDEBUG

parse' :: Parser a -> S.ByteString -> Either String a
parse' p str = go (parse p str)
 where
   go (Done str' r)
     | S.null str' = Right r
     | otherwise   = Left "no full parse"
   go (Fail _ ctxs msg) = Left $ msg ++ " " ++ show ctxs
   go (Partial f) = go (f S.empty)

test_Scion_Server_Message :: Test
test_Scion_Server_Message =
  testGroup "Scion.Server.Message"
  [ testGroup "decode/encode"
    [ testProperty "decode/encode 1" prop_decenc1
    , testProperty "decode/encode 2" prop_decenc2
    ]
  , testGroup "JSON"
    [ testCase "utf8 1" $
        parse' utf8Char (S.pack [0x24]) @=? Right '$'
    , testCase "utf8 2" $
        parse' utf8Char (S.pack [0xC2,0xA2]) @=? Right (chr 0xa2)
    , testCase "utf8 3" $
        parse' utf8Char (S.pack [0xE2,0x82,0xAC]) @=? Right (chr 0x20AC)
    , testCase "utf8 4" $
        parse' utf8Char (S.pack [0xF0,0xA4,0xAD,0xA2]) @=? Right (chr 0x024B62)
    , test_json "  null   " MsgNull
    , test_json "null" MsgNull
    , test_json "true" (MsgBool True)
    , test_json "false" (MsgBool False)
    , test_json "0" (MsgInt 0)
    , test_json "42" (MsgInt 42)
    , test_json "-42" (MsgInt (-42))
    , test_json "42e5" (MsgInt (4200000))
    , test_json "42E5" (MsgInt (4200000))
    , test_json "42.1e5" (MsgInt (4210000))
    , test_json "42.13e2" (MsgInt 4213)
    , test_json "-42.1e+5" (MsgInt (-4210000))
    , test_json "42e-5" (MsgDouble 0.00042)
    , test_json "-42e-5" (MsgDouble (-0.00042))
    , test_json "0e-5" (MsgInt 0)
    , test_json "0.0e-5" (MsgInt 0)
    , test_json "0.0e-0" (MsgInt 0)
    , test_json "42.32e-3" (MsgDouble 0.04232)
    , test_json "42.32e1" (MsgDouble 423.2)
    , test_json "-42.32e1" (MsgDouble (-423.2))
    , test_json "\"\"" (MsgText "")
    , test_json "\"foo\"" (MsgText "foo")
    , test_json "\"foo\\\\bar\"" (MsgText "foo\\bar")
    , test_json "\"foo\\nbar\"" (MsgText "foo\nbar")
    , test_json "\"fo\\u0020o\"" (MsgText "fo o")
    , test_json "\"fo\128x\"" (MsgText "fo\65533x")  -- invalid encoding
    , test_json_fail "\"fo\\u20o\""
    , test_json "[]" (MsgList [])
    , test_json "[true, null, false]"
        (MsgList [MsgBool True, MsgNull, MsgBool False])
    , test_json "{}" (MsgMap M.empty)
    , test_json "{ \"foo\" : 42 }"
        (MsgMap (M.fromList [("foo",MsgInt 42)]))
    , test_json "{ \"foo\" : 42 , \"bar\" : {} }"
        (MsgMap (M.fromList [("foo",MsgInt 42),
                             ("bar",MsgMap M.empty)]))
    , test_json_fail "nil"
    , test_json_fail "[null\"foo\"]"
    , test_json_enc MsgNull "null"
    , test_json_enc (MsgBool True) "true"
    , test_json_enc (MsgBool False) "false"
    , test_json_enc (MsgInt 42) "42"
    , test_json_enc (MsgDouble 4.2) "4.2"
    , test_json_enc (MsgDouble 1.5e-10) "1.5e-10"
    , test_json_enc (MsgList [42]) "[42]"
    , test_json_enc (MsgList [1,2,3]) "[1,2,3]"
    , test_json_enc "foo" "\"foo\""
    , test_json_enc "fo\"o" "\"fo\\\"o\""
    , test_json_enc (mkMap [("foo",42), ("bar","baz")])
                    "{\"bar\":\"baz\",\"foo\":42}"
    ]
  , testGroup "S-Expr"
    [ test_lisp "nil" MsgNull
    , test_lisp "t" (MsgBool True)
    , test_lisp "null" (MsgText "null")
    , test_lisp ":kw" (MsgText "kw")
    , test_lisp "42" (MsgInt 42)
    , test_lisp "-42" (MsgInt (-42))
    , test_lisp "42.0" (MsgInt 42)
    , test_lisp "4.2" (MsgDouble 4.2)
    , test_lisp "\"foo\"" (MsgText "foo")
    , test_lisp "\"foo\\\"bar\"" (MsgText "foo\"bar")
    , test_lisp "()" (MsgList [])
    , test_lisp "(nil)" (MsgList [MsgNull])
    , test_lisp "(())" (MsgList [MsgList []])
    , test_lisp "(:key 42)" (MsgMap (M.fromList [("key", MsgInt 42)]))
    ]
  ]

test_json txt rslt =
  testCase txt $ parse' parseJson (fromString txt) @?= Right rslt

test_json_enc inp rslt =
  testCase ("encode: " ++ rslt) $
    B.toLazyByteString (encodeJson inp) @?= L.fromChunks [fromString rslt]

test_json_fail txt =
  testCase ("no parse: " ++ txt) $ is_fail $ parse' parseJson (fromString txt)
 where
   is_fail (Left _) = assert True
   is_fail (Right r) = False @? "Expected failure but got: " ++ show r

test_lisp txt rslt =
  testCase txt $ parse' parseLisp (fromString txt) @?= Right rslt

-- TODO: Either turn into HUnit test case or add arguments.
prop_decenc1 =
  (decode . encode $ MsgMap (M.fromList [("a",MsgInt 33), ("0",MsgBool True)]))
    == MsgMap (M.fromList [("0",MsgBool True),("a",MsgInt 33)])

prop_decenc2 =
  (decode . encode $ MsgList [MsgInt 42, MsgText "foo"])
    == MsgList [MsgInt 42, MsgText "foo"]



#endif
