{-# LANGUAGE DeriveDataTypeable, OverloadedStrings,
             ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Scion.Server where

import Scion.Server.Message

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Control.Applicative
import Control.Concurrent
import Control.Exception ( Exception, bracketOnError, throwIO, handle )
import Control.Monad ( unless, forever ) 
import Data.Maybe ( fromMaybe, isJust )
import Data.Typeable ( Typeable )
import System.Directory ( doesFileExist )
import System.IO
import System.Process
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)

data WorkerHandle = WorkerHandle
  { workerStdin   :: Handle
  , workerStdout  :: Handle
  , workerStderr  :: Handle
  , workerProcess :: ProcessHandle
  , workerFlags   :: [String]
  }
instance Show WorkerHandle where
  show w =
    "<worker in:" ++ show (workerStdin w) ++ " out:" 
    ++ show (workerStdout w) ++ ">"
  
data CannotStartWorker = CannotStartWorker String
  deriving (Show, Typeable)
instance Exception CannotStartWorker

-- | Start a worker process.
--
-- Blocks until the worker is ready.
startWorker :: FilePath -- ^ Executable file for the worker.
            -> [String] -- ^ GHC API flags for the worker.
            -> IO WorkerHandle
startWorker executable api_args = do
  exists <- doesFileExist executable
  if not exists then
    throwIO $ CannotStartWorker "Executable does not exist"
   else
     bracketOnError
       (runInteractiveProcess executable api_args Nothing Nothing)
       close_all $
        \(inp, out, err, proc) -> do
          hSetBinaryMode inp True
          hSetBinaryMode out True
          -- Wait for worker to start up.
          rdy <- S.hGet out 5
          if rdy == C.pack "READY" then do
            return $ WorkerHandle
              { workerStdin = inp
              , workerStdout = out
              , workerStderr = err
              , workerProcess = proc
              , workerFlags = api_args 
              }
           else
            throwIO $ CannotStartWorker "Wrong worker or worker version"
 where
   close_all (inp, out, err, _) =
     hClose inp >> hClose out >> hClose err
     
-- | Stop a worker with optional timeout (in ms).
--
-- Send the worker a @quit@ message.  If it doesn't respond within the
-- specified timeout terminate its process.  A timeout of @0@
-- terminates the process immediately.
--
-- Note: This function does not block; it returns immediately.  You
-- can block on the returned 'MVar' to wait for the server to exit.
stopWorker :: 
     WorkerHandle
  -> Maybe Int -- ^ Timeout in milliseconds.  If @Nothing@ a
               -- default will be used (currently 60s).
  -> IO (MVar ())
     -- ^ The returned 'MVar' is written to when the server actually
     -- stopped.
stopWorker h mb_timeout = do
  stopped <- newEmptyMVar
  outlock <- newMVar ()
--   let pr :: Show a => a -> IO ()
--       pr x = withMVar outlock $ \_ -> print x
  let timeout = fromMaybe (60 * 1000) mb_timeout

--   err <- forkIO $ handle (\(e :: IOError) -> return ()) $
--            forever $ 
--              S.hGetLine (workerStderr h) >>= print . (,) "out"
          
  thr <- forkIO $ do
--            pr "sending"
           sendMessageToHandle (workerStdin h) $
             mkMap [("method", "quit")
                   ,("params", MsgNull)
                   ,("id", 0)]
--            pr "receiving"
--            pr =<< hRecv (workerStdout h) 32
--            pr =<< hIsOpen (workerStdout h)
           _ <- recvMessageFromHandle (workerStdout h)
           tryPutMVar stopped () >> return ()
  _ <- forkIO $ do
    let exact_timeout_us = fromIntegral timeout * 1000 :: Integer
        timeout_us
          | exact_timeout_us > fromIntegral (maxBound :: Int) =
            maxBound
          | otherwise =
            fromIntegral exact_timeout_us
    threadDelay timeout_us
    exited <- getProcessExitCode (workerProcess h)
    --pr exited
    unless (isJust exited) $ do
      --str <- S.hGetNonBlocking (workerStderr h) 200
      --pr str
      terminateProcess (workerProcess h)
    killThread thr
--    killThread err
    tryPutMVar stopped () >> return ()
  return stopped

-- | Concurrently read lines from the handle until action completes.
-- 
-- Runs the given 'IO' computation and concurrently reads lines from
-- the handle until the 'IO' computation finishes.
collectLines ::
     Handle -- ^ The handle to read from.
  -> IO a -- ^ The computation to run.
  -> IO (a, L.ByteString)
     -- ^ Result of the computation and the output that was read while
     -- the computation was running.
collectLines h act = do
  chunks_var <- newMVar []
  collector <- forkIO $ loop chunks_var
  result <- act
  lines <- takeMVar chunks_var  -- blocks the thread if necessary
  killThread collector
  return (result, L.fromChunks $ reverse lines)
 where
   loop var =
     handle (\(e :: IOError) -> return ()) $ do
       hWaitForInput h (-1)
       modifyMVar_ var $ \cs -> do
         chunk <- S.hGetNonBlocking h 4096
         return (chunk:cs)
       loop var

-- | Invoke an operation on the worker.  Waits for worker to respond.
--
-- Returns the worker's response and the output it generated.
callWorker :: WorkerHandle -> MsgData
           -> IO (MsgData, L.ByteString)
callWorker h request = do
  collectLines (workerStderr h) $ do
    sendMessageToHandle (workerStdin h) request
    recvMessageFromHandle (workerStdout h)

defaultMain :: IO ()
defaultMain = do
  return ()

{-
-- | Attempt to listen on each port in the list.
--
-- Returns the first successful
listenOnOneOf :: [PortID] -> IO (Maybe Socket)
listenOnOneOf [] = return Nothing
listenOnOneOf (p:ps) =
  (Just <$> listenOn p) `catch`
    (\(ex :: IOError) -> listenOnOneOf ps)
-}