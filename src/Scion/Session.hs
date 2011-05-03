{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings,
             ScopedTypeVariables #-}
-- | Basic Ideas:
--
-- All we need to /describe/ a session is a 'SessionConfig'.  From
-- that we can reconstruct all internal state on demand.  Of course,
-- for efficiency we do lots of caching (preferably on disk).
--
-- Session state stored and managed by a separate process, the Scion
-- worker.  This causes a bit of overhead, but for most actions will
-- be negligible.
--
-- Most interactions will be of the form \"This file has changed,
-- please update the state\" or \"Give me this information based on
-- the current state.\"
--
module Scion.Session where

import Scion.Types.Compiler
import Scion.Types.Note
import Scion.Types.Session
import Scion.Types.Commands
import Scion.Types.Monad
--import Scion.Worker
import Scion.Utils.Convert
import Scion.Utils.IO
import Control.Exception ( bracketOnError, throwIO, handle )

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception ( throwIO )
import           Control.Monad ( when, unless, forever )
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import           Data.Char ( ord )
import           Data.Maybe
import           Data.Time.Clock ( getCurrentTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )
import           System.Directory ( doesFileExist, getTemporaryDirectory )
import           System.FilePath ( dropFileName, (</>), takeFileName )
import           System.IO
import           System.PosixCompat.Files ( getFileStatus, modificationTime )
import           System.Process ( getProcessExitCode, terminateProcess )

-- -------------------------------------------------------------------

-- | Create a new session for the given session config.
--
-- Starts a new worker and returns the associated session ID.
createSession :: SessionConfig
              -> ScionM SessionId
createSession sc0@FileConfig{ sc_fileName = file } = do
  ok <- io $ doesFileExist  file
  when (not ok) $
    io $ throwIO $ userError $ 
      "createSession: File does not exist: " ++ file
 
  mod_time <- convert . modificationTime <$> io (getFileStatus file)

  starter <- getWorkerStarter
  let working_dir = dropFileName file
      sc = sc0{ sc_fileName = takeFileName file }

  (whdl, rslt, graph) <- startWorker starter working_dir sc

  outdir0 <- io $ getTemporaryDirectory
  sid <- genSessionId
  let outdir = outdir0 </> show sid

  -- TODO: specify output directory to worker
  let sess0 = SessionState
        { sessionConfig = sc
        , sessionConfigTimeStamp = mod_time
        , sessionWorker = whdl
        , sessionOutputDir = outdir
        , sessionModuleGraph = graph
        , sessionLastCompilation = rslt
        }

  registerSession sid sess0
  return sid

createSession sc@EmptyConfig{} = do
  starter <- getWorkerStarter
  working_dir <- io $ getTemporaryDirectory
  (whdl, rslt, graph) <- startWorker starter working_dir sc
  outdir0 <- io $ getTemporaryDirectory
  sid <- genSessionId
  let outdir = outdir0 </> show sid
  timestamp <- convert <$> io getCurrentTime
  -- TODO: specify output directory to worker
  let sess0 = SessionState
        { sessionConfig = sc
        , sessionConfigTimeStamp = timestamp
        , sessionWorker = whdl
        , sessionOutputDir = outdir
        , sessionModuleGraph = graph
        , sessionLastCompilation = rslt
        }

  registerSession sid sess0
  return sid

-- | Stop the session and associated worker.
destroySession :: SessionId -> ScionM ()
destroySession sid = do
  sess <- getSessionState sid
  _ <- io $ stopWorker (sessionWorker sess) (Just 3)
  unregisterSession sid
  return ()

-- | Create a temporary session that is destroyed when the
-- continuation exits (normally or via an exception).
withSession :: SessionConfig -> (SessionId -> ScionM a) -> ScionM a
withSession sconf k = do
  sid <- createSession sconf
  k sid `gfinally` (do destroySession sid; unregisterSession sid)

-- | Return messages for each node.
sessionNotes :: SessionId -> ScionM Notes
sessionNotes sid = do
  compilationNotes . sessionLastCompilation <$> getSessionState sid

supportedLanguagesAndExtensions :: ScionM [Extension]
supportedLanguagesAndExtensions = do
  exts <- getExtensions
  case exts of
    Just e -> return e
    Nothing -> do
      withSession (EmptyConfig []) $ \sid -> do
        wh <- sessionWorker <$> getSessionState sid
        (ans, _) <- io $ callWorker wh Extensions
        case ans of
          AvailExtensions exts -> do
            setExtensions exts
            return exts

-- | Notify the worker that a file has changed.  The worker will then
-- update its internal state.
fileModified :: SessionId -> FilePath -> ScionM ()
fileModified sid path = do
  -- TODO: check whether file is actually part of module graph
  -- TODO: properly merge compilation results
  st <- getSessionState sid
  let wh = sessionWorker st
  (ans, _) <- io $ callWorker wh Reload
  case ans of
    CompResult rslt graph -> do
      modifySessionState sid $ \ss ->
        (ss{ sessionModuleGraph = graph
           , sessionLastCompilation = rslt }, ())



-- -------------------------------------------------------------------

-- Internal: mainly for testing purposes
ping :: SessionId -> ScionM Bool
ping sid = do
  st <- getSessionState sid
  let wh = sessionWorker st
  (ans, _) <- io $ callWorker wh Ping {-$ mkMap [("method", "ping")
                                  ,("params", MsgNull)
                                  ,("id", 42)]-}
  return $ case ans of Pong -> True; _ -> False --decodeKey ans "result" == Ok ("pong" :: T.Text)

-- Internal: targets are derived from the SessionConfig
setTargets :: SessionId -> [Target] -> ScionM ()
setTargets sid _targets = do
  st <- getSessionState sid
  let _targets = sessionTargets (sessionConfig st)
          
  return ()

sessionTargets :: SessionConfig -> [Target]
sessionTargets FileConfig{ sc_fileName = f} = [FileTarget f]
sessionTargets CabalConfig{} = [] 

-- -------------------------------------------------------------------

-- | Start a worker process.
--
-- Blocks until the worker is ready.
startWorker :: WorkerStarter
            -> FilePath -- ^ Working directory.
            -> SessionConfig
            -> ScionM (WorkerHandle, CompilationResult, [ModuleSummary])
startWorker start_worker homedir conf = do
  loglvl <- getLogLevel
  io $ bracketOnError
    (start_worker homedir [])
    close_all $
     \(inp, out, err, proc) -> do
       hSetBinaryMode inp True
       hSetBinaryMode out True
       if loglvl > 2 then forkIO (printFromHandle err) else return undefined
       -- Wait for worker to start up.
       wait_for_READY out

       sendMessageToHandle inp conf
       ok <- recvMessageFromHandle out
       --killThread dumper
       case ok of
         Nothing -> do
           threadDelay 2000000
           throwIO $ CannotStartWorker "Wrong worker or worker version"
         Just (rslt :: CompilationResult, graph :: [ModuleSummary]) ->
           return 
             (WorkerHandle { workerStdin = inp
                           , workerStdout = out
                           , workerStderr = err
                           , workerProcess = proc
                           , workerFlags = []
                           },
              rslt, graph)
 where
   close_all (inp, out, err, _) =
     hClose inp >> hClose out >> hClose err
   wait_for_READY h = do
     handle (\(_e :: IOError) -> putStrLn "Could not start worker.") $ do
       l <- S.hGetLine h
       if l == str_READY then return () else do
         -- ignore other lines
         putStrLn $ "Worker: " ++ show l
         wait_for_READY h

   str_READY = S.pack (map (fromIntegral . ord) "READY")
   printFromHandle hdl =
     handle (\(_e :: IOError) -> return ()) $ do
       forever $ do
         hWaitForInput hdl (-1)
         s <- S.hGetNonBlocking hdl 256
         hPutStr stderr (show hdl ++ ": ")
         S.hPutStr stderr s

-- | Stop a worker with optional timeout (in milliseconds).
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
  let timeout = fromMaybe (60 * 1000) mb_timeout

  thr <- forkIO $ do
           sendMessageToHandle (workerStdin h) Quit
           (_ :: Maybe Answer) <- recvMessageFromHandle (workerStdout h)
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
    unless (isJust exited) $ do
      terminateProcess (workerProcess h)
    killThread thr
    tryPutMVar stopped () >> return ()
  return stopped

-- | Concurrently read lines from the handle until action completes.
-- 
-- Runs the given 'IO' computation and concurrently reads lines from
-- the handle until the 'IO' computation returns.
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
  lines_ <- takeMVar chunks_var  -- blocks the thread if necessary
  killThread collector
  return (result, L.fromChunks $ reverse lines_)
 where
   loop var =
     handle (\(_e :: IOError) -> return ()) $ do
       hWaitForInput h (-1)
       modifyMVar_ var $ \cs -> do
         chunk <- S.hGetNonBlocking h (2*4096)
         return (chunk:cs)
       loop var

-- | Invoke an operation on the worker.  Waits for worker to respond.
--
-- Returns the worker's response and the output it generated.
callWorker :: WorkerHandle -> Command -> IO (Answer, L.ByteString)
callWorker h request = do
  collectLines (workerStderr h) $ do
    sendMessageToHandle (workerStdin h) request
    ans_ <- recvMessageFromHandle (workerStdout h)
    case ans_ of 
      Just ans -> return ans
      Nothing -> return (Error "callWorker: Could not parse answer")
