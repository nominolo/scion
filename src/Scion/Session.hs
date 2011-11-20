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
import Scion.Cabal ( CabalException )

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad ( when, unless, forever, filterM )
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import           Data.Char ( ord )
import           Data.Maybe
import           Data.Time.Clock ( getCurrentTime )
import           System.Directory ( doesFileExist, getTemporaryDirectory,
                                    removeDirectoryRecursive )
import           System.Exit ( ExitCode(..) )
import           System.FilePath ( dropFileName, (</>), takeFileName,
                                   takeDirectory )
import           System.FilePath.Canonical
import           System.IO
import           System.IO.Temp ( createTempDirectory )
import           System.PosixCompat.Files ( getFileStatus, modificationTime )
import           System.Process ( getProcessExitCode, terminateProcess )

-- -------------------------------------------------------------------

-- | Throw a 'ScionException' if the file does not exist.
ensureFileExists :: FilePath -> ScionM ()
ensureFileExists file = do
  ok <- io $ doesFileExist  file
  when (not ok) $ scionError $ "File does not exist: " ++ file

-- | Create a new session for the given session config.
--
-- Starts a new worker and returns the associated session ID.
createSession :: SessionConfig
              -> ScionM SessionId
createSession sc0@FileConfig{ sc_fileName = file } = do
  ensureFileExists file
 
  mod_time <- convert . modificationTime <$> io (getFileStatus file)

  starter <- getWorkerStarter
  working_dir <- io $ canonical $ dropFileName file
  let sc = sc0{ sc_fileName = takeFileName file }

  (whdl, rslt, graph) 
    <- startWorker starter (canonicalFilePath working_dir) sc

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
        , sessionHomeDir = working_dir
        }

  registerSession sid sess0
  return sid

createSession sc0@CabalConfig{ sc_cabalFile = file } = do
  ensureFileExists file
 
  mod_time <- convert . modificationTime <$> io (getFileStatus file)

  starter <- getWorkerStarter
  working_dir <- io $ canonical $ dropFileName file

  sid <- genSessionId
  
  build_dir <- case sc_buildDir sc0 of
                 Nothing -> do
                   tmp <- io getTemporaryDirectory
                   dir <- io $ createTempDirectory tmp "scion-dist"
                   addCleanupTodo (removeDirectoryRecursive dir)
                   return dir
                 Just d -> return d

  let sc = sc0{ sc_buildDir = Just build_dir,
                sc_cabalFile = takeFileName file -- TODO: use absolute path instead
              }
  (whdl, rslt, graph)
    <- startWorker starter (canonicalFilePath working_dir) sc

  let sess0 = SessionState
        { sessionConfig = sc
        , sessionConfigTimeStamp = mod_time
        , sessionWorker = whdl
        , sessionOutputDir = build_dir
        , sessionModuleGraph = graph
        , sessionLastCompilation = rslt
        , sessionHomeDir = working_dir
        }

  registerSession sid sess0
  return sid
  

createSession sc@EmptyConfig{} = do
  starter <- getWorkerStarter
  working_dir <- io $ canonical =<< getTemporaryDirectory
  (whdl, rslt, graph)
    <- startWorker starter (canonicalFilePath working_dir) sc
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
        , sessionHomeDir = working_dir
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

sessionModules :: SessionId -> ScionM [ModuleSummary]
sessionModules sid = sessionModuleGraph <$> getSessionState sid

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
          AvailExtensions exts' -> do
            setExtensions exts'
            return exts'
          _ -> fail "supportedLanguagesAndExtensions: invalid answer"

-- | Notify the worker that a file has changed.  The worker will then
-- update its internal state.
fileModified :: SessionId -> FilePath -> ScionM ()
fileModified sid _path = do
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
    _ -> fail "fileModified: invalid answer"



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
sessionTargets _ = []

-- -------------------------------------------------------------------

-- | Start a worker process.
--
-- Blocks until the worker is ready.
startWorker :: WorkerStarter
            -> FilePath -- ^ Working directory.
            -> SessionConfig
            -> ScionM (WorkerHandle, CompilationResult, [ModuleSummary])
startWorker start_worker homedir conf = do
  verb <- getVerbosity
  io $ bracketOnError
    (start_worker homedir [])
    close_all $
     \(inp, out, err, proc) -> do
       hSetBinaryMode inp True
       hSetBinaryMode out True
       _ <- if verb >= deafening then forkIO (printFromHandle err) else return undefined
       -- Wait for worker to start up.
       wait_for_READY out

       sendMessageToHandle inp conf
       ok <- recvMessageFromHandle out
       --killThread dumper
       case ok of
         Nothing -> do
           threadDelay 2000000
           throwIO $ CannotStartWorker "Wrong worker or worker version"
         Just (Left msg) -> do
           scionError $ "Worker error: " ++ msg
         Just (Right (rslt :: CompilationResult, graph :: [ModuleSummary])) ->
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
         _ <- hWaitForInput hdl (-1)
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
       _ <- hWaitForInput h (-1)
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

ignoreMostErrors :: (ExceptionMonad m, MonadIO m) =>
                     m a -> m (Either String a)
ignoreMostErrors act = do
  gcatches (act >>= return . Right)
    [HandlerM $ \(ex :: CabalException) -> return (Left (show ex)),
     HandlerM $ \(ex :: ExitCode) -> return (Left (show ex)),
     HandlerM $ \(ex :: IOError) -> return (Left (show ex)),
     HandlerM $ \(ex :: ScionException) -> return (Left (show ex)),
     HandlerM $ \(ex :: PatternMatchFail) -> return (Left (show ex)),
     HandlerM $ \(ex :: ErrorCall) -> return (Left (show ex)),
     HandlerM $ \(ex :: RecConError) -> return (Left (show ex))
    ]


-- | Find the (active) sessions that the given file is part of.
fileSessions :: FilePath -> ScionM [SessionId]
fileSessions path = do 
  filterM (fileInSession path) =<< activeSessions

fileInSession :: FilePath -> SessionId -> ScionM Bool
fileInSession path0 sid = do
  home <- sessionHomeDir <$> getSessionState sid
  path <- io $ canonical $ canonicalFilePath home </> path0
  mods <- sessionModules sid
  return $ not $ null [ m | m <- mods, ms_location m == path ]

-- | Find a session for the given configuration (if any).
--
-- This uses linear search, so the assumption is that there won't be
-- too many sessions active at any one time.
--
-- Note that no normalisation of any flags specified inside the
-- session occurs.  So searching for an existing session with possibly
-- different flag assignments will fail.
sessionForConfig :: SessionConfig -> ScionM (Maybe SessionId)
sessionForConfig conf_ = do
  sessions <- activeSessionsFull
  let (conf, path_) = normaliseConf conf_
  path <- io $ canonical path_
  --message silent $ "Sessions: " ++ show conf ++ "\n" ++
  --        show (map (sessionConfig . snd) (M.toList sessions))
  case [ sid | (sid, s) <- M.toList sessions
             , sessionConfig s == conf &&
               (if path_ /= "" then path == sessionHomeDir s else True) ]
   of [] -> return Nothing
      (sid:_) -> return (Just sid)
 where
   normaliseConf c@FileConfig{ sc_fileName = f } =
     (c{ sc_fileName = takeFileName f }, takeDirectory f)
   normaliseConf c@CabalConfig{ sc_cabalFile = f } =
     (c{ sc_cabalFile = takeFileName f }, takeDirectory f)
   normaliseConf c = (c, "")