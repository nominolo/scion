{-# LANGUAGE CPP, OverloadedStrings, PatternGuards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Main where

--import Scion.Types
import Scion.Server
import Scion.Server.Options
import Scion.Server.Message
--import Scion.Server.Commands2

-- import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Binary.Builder as B
import qualified Data.Map as M
import Data.Text ()
import Data.String ( fromString )
import Control.Applicative
import Control.Concurrent
import Control.Exception ( Exception, bracket, bracketOnError, throwIO, handle, IOException )
--import Control.Monad ( when, unless, forever ) 
--import Data.Maybe ( fromMaybe, isJust )
--import Data.Typeable ( Typeable )
import Network ( PortID(..), listenOn )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.IO
import Data.IORef
--import System.Process


#ifndef NDEBUG
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )
--import Test.Framework.Providers.QuickCheck2 (testProperty)
--import Test.QuickCheck hiding ( (.&.) )

import System.Environment ( withArgs )
--import Debug.Trace
#endif


main :: IO ()
main = do
  config <- parseArgs
  case config of
    StdIO{} -> do
      hSetBuffering stdout LineBuffering
      hSetBuffering stdin LineBuffering
      hSetBuffering stderr LineBuffering
      handleClient (wire_format config) (worker_path config)
        (return . L.fromChunks . (:[]) =<< C.hGetLine stdin)
        (\l -> L.hPut stdout l >> L.hPut stdout nl >> hFlush stdout)
    Socket{} -> withSocketsDo $ do
      sock <- if scan_port config then
                listenOnOneOf (map (PortNumber . fromIntegral) 
                                   (port config : dynamic_ports))
               else
                listenOn (PortNumber (fromIntegral (port config)))
      real_port <- socketPort sock
      putStrLn $ "=== Listening on port: " ++ show real_port
      hFlush stdout
      handle (\(e :: IOException) -> do
                putStrLn $ "Caught: " ++ show e) $ do
        (sock', _addr) <- accept sock
        sock_conn <- mkSocketConn sock'
        handleClient (wire_format config) (worker_path config)
          (getLineSC sock_conn) (putLineSC sock_conn)
          
 where
   nl = L.singleton '\n'
   dynamic_ports = [0xc000 .. 0xffff]

-- | A socket plus a buffer.  Needed for line-based socket I\/O.
data SocketConn = SocketConn Socket (IORef C.ByteString)

mkSocketConn :: Socket -> IO SocketConn
mkSocketConn sock = SocketConn sock `fmap` newIORef C.empty

-- | Read a line or until the end of the stream.
getLineSC :: SocketConn -> IO L.ByteString
getLineSC (SocketConn sock buf_ref) = do
  (line_chunks, buf') <- go =<< readIORef buf_ref
  writeIORef buf_ref buf'
  return (L.fromChunks line_chunks)
 where
   chunk_size = 1024
   go buf | C.null buf = do
     chunk <- recv sock chunk_size
     if C.null chunk
       then return ([], C.empty)
       else go chunk
   go buf =
     let (before, rest) = C.break (=='\n') buf in
     if C.null rest then do
       (chunks, buf') <- go rest
       return (before : chunks, buf')
      else
        return ([before], C.drop 1 rest)

putLineSC :: SocketConn -> L.ByteString -> IO ()
putLineSC (SocketConn sock _) str = go (L.toChunks str)
 where
   newline = C.singleton '\n'
   go [] = send sock newline >> return ()
   go (chunk:chunks) = do
     let l = C.length chunk
     sent_bytes <- send sock chunk
     if (sent_bytes < l) then
       hPutStrLn stderr $ "Error: Tried to send " ++ show l ++ " bytes, but could only send: " ++ show sent_bytes
      else go chunks  

-- | Attempt to listen on each port in the list.
--
-- Returns the socket for the first free port found.  Re-throws the
-- connection error in case no port in the list was free.
listenOnOneOf :: [PortID] -> IO Socket
listenOnOneOf (p:ps) =
  listenOn p `catch`
    (\(ex :: IOError) ->
       if null ps then throwIO ex else listenOnOneOf ps)

-- | The main server loop.
--
-- TODO: Allow multiple workers and make sure that we dispatch
-- messages to the right worker.
handleClient :: 
     WireFormat
  -> FilePath
  -> IO L.ByteString  -- ^ Reads a line from the client.
  -> (L.ByteString -> IO ()) -- ^ Sends a line to the client.
  -> IO ()
handleClient format worker_fn get_line put_line = do
  bracket (startWorker worker_fn [])
    (\worker -> stopWorker worker (Just 5000)) $ \worker -> do
      server_loop worker
 where
   server_loop worker = do
     msg_text <- get_line
     --hPutStrLn stderr $ "Input:" ++ show msg_text
     case decodeWireMessage format msg_text of
       Left msg -> do
         -- FIXME: Send error message in correct format.
         --hPutStrLn stderr $ "Decode error: " ++ msg
         put_line (encodeWireMessage format (parseErrorMessage msg))
         server_loop worker
       Right msg 
         | MsgMap mp <- msg,
           Just meth <- M.lookup "method" mp,
           meth == "quit" -> do
             return ()
         | otherwise -> do
             -- TODO: Pick the right worker based on the message.
             (ans, _) <- callWorker worker msg
             put_line (encodeWireMessage format ans)
             server_loop worker

decodeWireMessage :: WireFormat -> L.ByteString
                  -> Either String MsgData
decodeWireMessage Json text =
  parseLazyBS (parseJson <* endOfInput) text
decodeWireMessage Lisp text =
  parseLazyBS (parseLisp {- <* endOfLine -} <* endOfInput) text

encodeWireMessage :: WireFormat -> MsgData -> L.ByteString
encodeWireMessage Json msg =
  B.toLazyByteString (encodeJson msg)
  
parseErrorMessage :: String -> MsgData
parseErrorMessage parse_err =
  mkMap [("error",
          mkMap [("name", "MessageParseError")
                ,("message", "Could not parse message.")
                ,("info", fromString parse_err )])
        ,("version",2)]

t1 :: IO ()
t1 = withFile "/Users/ts319/tstinp" WriteMode $ \hdl -> do
       sendMessageToHandle hdl $
         mkMap [("method", "quit")
               ,("params", MsgNull)
               ,("id", 0)]

#ifndef NDEBUG
test_worker_path :: FilePath
test_worker_path = "/Users/ts319/tmp/dist-cabal/scion_6.12.1/manual/worker"

withWorker :: (WorkerHandle -> IO a) -> IO ()
withWorker act = do
  h <- startWorker test_worker_path []
  act h
  readMVar =<< stopWorker h (Just 5000)
  
test_Proxy :: Test
test_Proxy =
  testGroup "Scion.Server.Proxy"
  [ testCase "simple" $ do
      withArgs [] $ do
        opts <- parseArgs
        wire_format opts @?= Json
  , testCase "start-worker" $ do
      hdl <- startWorker test_worker_path []
      s <- stopWorker hdl (Just 5000)
      readMVar s
  , testCase "ping-worker" $ do
      withWorker $ \h -> do
        (r,_) <- callWorker h $ mkMap [("method", "ping")
                                     ,("params", MsgNull)
                                     ,("id", 0)]
        r @?= mkMap [("result", MsgNull)
                    ,("id", 0)
                    ,("version", 2)]
  , testCase "unknown-command" $
      withWorker $ \h -> do
        (r,_) <- callWorker h $ mkMap [("method", "ekaokaeosniuh")
                                      ,("params", MsgNull)
                                      ,("id", 1)]
        r @?= mkMap [("error",
                      mkMap [("name", "UnknownCommand")
                            ,("message", "The requested method is not supported.")])
                    ,("version", 2)
                    ,("id", 1)]
  ]
#endif
