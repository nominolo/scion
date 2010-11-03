{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
-- | The main loop for a Scion worker.
--
-- A worker communicates with the Scion server via the standard input
-- and standard output pipes.  The wire format is binary data, so we
-- have to make sure that no library call accidentally prints to
-- stdout as well.  For that reason we redirect all output to 'stdout'
-- to 'stderr' and keep an exclusive handle to the standard output
-- pipe that cannot be accessed by client code.
--
-- The controlling process must indeed read from stderr.  Otherwise
-- the process may block because the write buffer is full.  The
-- simplest way of doing this would be to spawn off a Haskell thread
-- that just continuously reads from 'stderr' and discards the input.
--
-- The command line arguments to the worker are the flags to pass to
-- the GHC API and may include static flags (i.e., flags that can only
-- be set once during the run time of the program).
--
-- Error handling is deliberately crude.  Workers are considered
-- internal so we don't worry about user-friendliness.  If anything
-- goes wrong we just let the worker die.  The Scion server will then
-- decide what to do.
module Scion.Server.Worker
  ( workerMain
#ifndef NDEBUG
--  , test_Scion_Server_Worker 
#endif
) where

import Scion.Server.Message
import Scion.Server.Commands2
import Scion

--import Network ( withSocketsDo, connectTo )
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Control.Exception (bracket)
import Control.Monad ( when )
import Data.Maybe ( isJust )
import GHC.IO.Handle ( hDuplicate, hDuplicateTo )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import System.Environment
import System.IO

#ifndef NDEBUG
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding ( Test )
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
#endif

-- | The worker's default implementation.
--
-- It communicates with the server using @stdin@ and @stdout@.  Any
-- terminal output from the worker is redirected to @stderr@.
--
-- TODO: This intermingles debug output with normal output.  A
-- possible improvement could be to implement a custom handle, that
-- just accumulates the output into ByteString and sends this with the
-- response.  Currently, this is implemented by the server\/proxy.
workerMain :: IO ()
workerMain = do
  api_args <- getArgs
  let inp = stdin
  out <- makeExclusive stdout stderr
  mapM_ ensureBinaryMode [inp, out]
  hSetBuffering stderr LineBuffering

  runScion' api_args $ do
    -- Signal that the client is ready.
    liftIO $ do
      S.hPut out (C.pack "READY")
      hFlush out

    workerLoop inp out

  hFlush stderr
 where
   workerLoop inp out = do
     msg <- liftIO $ recvMessageFromHandle inp
     --liftIO $ (putStrLn $ "=> " ++ show msg) >> hFlush stdout
     (ans, keep_going) <- handleRequest msg
     --liftIO $ (putStrLn $ "<= " ++ show ans) >> hFlush stdout
     liftIO $ sendMessageToHandle out ans
     if keep_going then workerLoop inp out else return ()

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
  hresult <- hDuplicate hexcl
  hDuplicateTo hredirect hexcl
  return hresult
  
-- | Ensure that the handle is in binary mode.
ensureBinaryMode :: Handle -> IO ()
ensureBinaryMode h = do
  enc <- hGetEncoding h
  when (isJust enc) $
    hSetBinaryMode h True



#ifndef NDEBUG
{-
test_Scion_Server_Worker :: Test
test_Scion_Server_Worker =
  testGroup "Scion.Server.Worker"
  [ testCase "send+recvMessage/1" test_1
  , testCase "send+recvMessage/2" test_2
  ]
 where
   test_1 = connectedTest client server
    where
      client sock = do
        msg <- recvMessage sock
        msg @?= MsgInt 42
      server sock = do
        sendMessage sock (MsgInt 42)

   test_2 = connectedTest client server
    where
      client sock = do
        msg <- recvMessage sock
        msg @?= MsgInt 42
        sendMessage sock (MsgInt 43)
      server sock = do
        sendMessage sock (MsgInt 42)
        msg <- recvMessage sock
        msg @?= MsgInt 43
-}
test_port :: PortNumber
test_port = fromIntegral (3000 :: Int)

-- from network-bytestring/test/Simple.hs

-- | Run a client/server pair and synchronize them so that the server
-- is started before the client and the specified server action is
-- finished before the client closes the connection.
connectedTest :: (Socket -> IO a) -> (Socket -> IO b) -> IO ()
connectedTest clientAct serverAct = do
    barrier <- newEmptyMVar
    forkIO $ server barrier
    client barrier
  where
    server barrier = do
        addr <- inet_addr "127.0.0.1"
        bracket (socket AF_INET Stream defaultProtocol) sClose $ \sock -> do
            setSocketOption sock ReuseAddr 1
            bindSocket sock (SockAddrInet test_port addr)
            listen sock 1
            serverReady
            (clientSock, _) <- accept sock
            serverAct clientSock
            sClose clientSock
            putMVar barrier ()
      where
        -- | Signal to the client that it can proceed.
        serverReady = putMVar barrier ()

    client barrier = do
        takeMVar barrier
        bracket (socket AF_INET Stream defaultProtocol) sClose $ \sock -> do
            addr <- inet_addr "127.0.0.1"
            connect sock $ SockAddrInet test_port addr
            clientAct sock
            takeMVar barrier

#endif
