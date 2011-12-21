{-# LANGUAGE CPP, ScopedTypeVariables #-}
-- | This is the implementation of a worker.
--
module Scion.Worker.Main
  ( workerMain, soloWorkerMain )
where

import Scion.Types.Note
import Scion.Types.Worker
import Scion.Types.Session
import Scion.Utils.IO
import Scion.Utils.Convert
import Scion.Worker.Commands
import Scion.Ghc

import qualified GHC as Ghc
import DynFlags as Ghc
import GHC.Paths ( libdir, ghc, ghc_pkg )
import Outputable ( ppr, showSDoc, withPprStyle, SDoc )
import qualified Outputable as O

import qualified Distribution.Compiler as C
import qualified Distribution.Simple.Configure as C
import qualified Distribution.Simple.Build as C
import qualified Distribution.PackageDescription as C
import qualified Distribution.PackageDescription.Parse as C
import qualified Distribution.Verbosity as C
import qualified Distribution.Simple.GHC as C hiding ( configure )
import qualified Distribution.Simple.Command as C
import qualified Distribution.Simple.Setup as C
import qualified Distribution.Simple.Program as C
import qualified Distribution.Simple.LocalBuildInfo as C
import qualified Distribution.Simple.Utils as C

import qualified Data.ByteString.Char8 as C
import Data.Time.Clock
import Data.List ( find )
import Data.String ( fromString )
import Control.Applicative
import Control.Concurrent ( threadDelay )
import Data.IORef
import System.Environment
import System.FilePath
import System.IO
import System.IO.Unsafe ( unsafePerformIO )
import System.Directory hiding ( getModificationTime )
import System.PosixCompat.Files ( getFileStatus, modificationTime )
import System.FilePath.Canonical

#if __GLASGOW_HASKELL__ < 702
import qualified Distribution.Simple.PreProcess as C ( knownSuffixHandlers )
#endif

------------------------------------------------------------------------
--
-- Compilation worker initialisation sequence:
--
--  1. Server creates worker process with one argument: the working
--     directory
--
--  2. Worker starts up and sends the ASCII bytes for "READY"
--
--  3. Server sends a SessionConfig.
--
--  4. Worker configures a Cabal project if necessary, then starts up
--     a GHC session and responds with the compilation result.
--
--  After that, the server sends commands and the worker responds.
--  Some commands may cause the worker to tell the server to restart
--  the worker.  For example:
--
--    - The user edited the .cabal file and changed some of the static
--      flags.  Flags can only be set once per process.
--
--    - The package database has changed.  It may be safe to just
--      restart the session, but I'm not sure.
--
------------------------------------------------------------------------

{-# NOINLINE logfile #-}
logfile :: Handle
logfile = unsafePerformIO $ do
            path <- getAppUserDataDirectory "scion"
            createDirectoryIfMissing True path
            openFile (path </> "worker-log") AppendMode

debugMsg :: MonadIO m => String -> m ()
debugMsg msg =
  liftIO (do hPutStrLn stderr msg >> hFlush stderr
             hPutStrLn logfile msg >> hFlush logfile)

workerMain :: Int -> IO ()
workerMain n =
  handle (\(e :: SomeException) -> do
            debugMsg ("Worker quit: " ++ show e)
            threadDelay 2000000) $
    workerMain' n

workerMain' :: Int -> IO ()
workerMain' _n = do
  -- 1. We will use stdin/stdout to communicate with the server.
  -- stderr will be used for logging.
  let inp = stdin
  out <- makeExclusive stdout stderr
  mapM_ ensureBinaryMode [inp, out]
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  hFlush stderr

  hPutStrLn stdout "test"
  debugMsg "=== Starting worker =============================="

  -- The arguments are the working directory and GHC flags.
  args <- getArgs
  debugMsg $ "Args: " ++ show args
  let worker_dir:_other_args = args
  setCurrentDirectory worker_dir
  debugMsg $ "Worker dir: " ++ worker_dir

  debugMsg "Sending READY"

  C.hPut out (C.pack "READY\n")
  hFlush out

  debugMsg "Receiving SessionConfig ..."

  msg0 <- recvMessageFromHandle inp
  case (msg0 :: Maybe SessionConfig) of
    Just sess_conf -> do
      debugMsg $ "OK: " ++ show sess_conf
      --let sess_conf = FileConfig {sc_fileName = "tests/projects/file001.hs", sc_flags = []}
      initWorker sess_conf debugMsg (main_loop inp out)
        `gcatch` (\(e :: SomeException) ->
                    sendMessageToHandle out (Left (show e) :: Ans0))
      return ()
    Nothing -> do
      debugMsg "ERROR"
      return ()

type Ans0 = Either String (CompilationResult, [ModuleSummary])

main_loop :: Handle -> Handle
          -> CompilationResult -> Worker ()
main_loop inp out rslt0 = do
--  initWorkerLogging debug
  graph <- moduleGraph
  liftIO $ sendMessageToHandle out (Right (rslt0, graph) :: Ans0)
--  liftIO $ sendMessageToHandle out "STARTUP_OK"
  loop
  --cleanupWorker
 where 
   loop = do
     msg_ <- liftIO $ recvMessageFromHandle inp
     case msg_ of
       Nothing -> do
         debugMsg $ "Could not decode message, exiting"
         return ()
       Just msg -> do
         debugMsg $ "in: " ++ show msg
         (ans, keep_going) <- handleRequest msg
         debugMsg $ "out: " ++ show ans
         liftIO $ sendMessageToHandle out ans
         if keep_going then loop else return ()

workerFail :: MonadIO m => String -> m a
workerFail msg =
  liftIO (hPutStrLn stderr msg >> hFlush stderr) >>
  error msg

-- | Start up a worker for the given session config.
initWorker :: SessionConfig
           -> (String -> IO ())
           -> (CompilationResult -> Worker a)  -- ^ The continuation (the main worker loop).
           -> IO a

initWorker EmptyConfig{ sc_flags = args0 } _debugMsg kont = do
  let args1 = map (Ghc.mkGeneralLocated ("<config:no-location>")) args0
  initGhcSession [] args1 debugMsg kont

initWorker FileConfig{ sc_fileName = file0, sc_flags = args0 } _debugMsg kont = do
  let args1 = map (Ghc.mkGeneralLocated ("<config:" ++ file0 ++ ">")) args0
  file <- (</> file0) <$> getCurrentDirectory
  debugMsg "Calling initGhcSession"
  initGhcSession [FileTarget file] args1 debugMsg kont

initWorker conf@CabalConfig{} _debugMsg kont = do
  -- TODO: read or create local build info in order to get to the
  -- command line arguments.  Then do same stuff as below.
  cabal_file <- (</> sc_cabalFile conf) <$> getCurrentDirectory
  cf_exists <- doesFileExist cabal_file
  if not cf_exists then workerFail $ "Cabal file not found: " ++ cabal_file
   else do
    let Just odir = sc_buildDir conf
    (lbi, _stamp) <- maybeConfigureCabal cabal_file (sc_configFlags conf) odir

    let comp = sc_component conf
    io $ print =<< getCurrentDirectory
        
    case getComponentInfo lbi comp of
      Nothing -> workerFail $ "Component `" ++ show comp
                   ++ "' not found in " ++ cabal_file
      Just (lib_or_exe, clbi, bi) -> do
        targets <-
          case lib_or_exe of
            Left lib ->
              return $
                map (ModuleTarget . convert) (C.libModules lib)
            Right exe -> do
              let mods =
                    map (ModuleTarget . convert) (C.exeModules exe)
              -- Cabal allows specifying "main-is: foo.hs", however if
              -- the real file actually is "src/foo.hs" then GHC won't
              -- find it.  So we have to manually find it here.
              main_file <- io $ C.findFile (C.hsSourceDirs bi) (C.modulePath exe)
              return (FileTarget main_file : mods)
                   
        let args = map Ghc.noLoc (C.ghcOptions lbi bi clbi odir)

        initGhcSession targets args debugMsg kont


-- TODO: refine behaviour based on GHC's ghc/Main.hs
initGhcSession :: [Target] -> [Ghc.Located String] 
               -> (String -> IO ())
               -> (CompilationResult -> Worker a) -> IO a
initGhcSession targets args1 _debugMsg kont = do
  -- TODO: check whether file exists
  debugMsg $ "GHC Args: " ++ show (map Ghc.unLoc args1)

  -- handles Ctrl-C and GHC panics and suchlike
#if __GLASGOW_HASKELL__ >= 702
  Ghc.defaultErrorHandler Ghc.defaultLogAction $ do
#else
  Ghc.defaultErrorHandler Ghc.defaultDynFlags $ do
#endif

    -- 1. Initialise all the static flags
    debugMsg "Parsing static flags"
    (args2, static_flag_warns) <- Ghc.parseStaticFlags args1
    debugMsg $ "Static flag warnings: " ++ 
              show (map (show . Ghc.unLoc) static_flag_warns)

    Ghc.runGhc (Just libdir) $ do
      
      -- 2. Now initialise the dynamic stuff
      dflags0 <- Ghc.getSessionDynFlags
      
      notes_ref <- liftIO $ newIORef []
      base_dir <- liftIO $ canonicalFilePath <$>
                    (canonical =<< getCurrentDirectory)
      
      let addNote :: NoteKind -> Ghc.SrcSpan -> SDoc -> IO ()
          addNote nkind loc msg =
            let note = Note { noteKind = nkind
                            , noteLoc = ghcSpanToLocation base_dir loc
                            , noteMessage = fromString (showSDoc msg) } in
            atomicModifyIORef notes_ref $ \ns ->
              (note : ns, ())
      

      let msg_text loc sty msg = 
            showSDoc (O.hang (ppr loc) 8 (withPprStyle sty msg))
            
          my_log_action severity loc sty msg = do
            case severity of
              --Ghc.SevOutput -> debugMsg $ "OUT: " ++ msg_text loc sty msg
              Ghc.SevWarning -> do
                --debugMsg $ "WARN: " ++ msg_text loc sty msg
                addNote WarningNote loc (withPprStyle sty msg)
              Ghc.SevError   -> do
                --debugMsg $ "ERR: " ++ msg_text loc sty msg
                addNote ErrorNote loc (withPprStyle sty msg)
              Ghc.SevInfo    -> debugMsg $ "INFO: " ++ msg_text loc sty msg
              Ghc.SevFatal   -> debugMsg $ "FATAL: " ++ msg_text loc sty msg
              _   -> debugMsg $ "OUT: " ++ msg_text loc sty msg
      let dflags1 =
            dflags0{ ghcMode = CompManager
                   , hscTarget = HscNothing
                   , ghcLink = LinkInMemory
                   , log_action = my_log_action
                   }

      (dflags2, _fileargs, dyn_flag_warns)
        <- Ghc.parseDynamicFlags dflags1 args2

      let flag_warns = static_flag_warns ++ dyn_flag_warns
      liftIO $ debugMsg $ "Flag warnings: " ++ 
             show (map (show . Ghc.unLoc) flag_warns)

      Ghc.defaultCleanupHandler dflags2 $ do

        _ <- Ghc.setSessionDynFlags dflags2
 
        let targets' = (map convert targets)
        liftIO $ debugMsg $ "Setting targets: " ++ show targets
        Ghc.setTargets targets' 

        r <- liftIO $ mkWorkerState notes_ref
        unWorker (load Ghc.LoadAllTargets >>= kont) r

-- | Configure Cabal project if necessary.  It is necessary if:
--
--  - No local build config exists, or
--
--  - The Cabal file is newer than the build config, or
--
--  - Other local build config dependencies changed (e.g., compiler
--    version, Cabal version, etc.)
--
maybeConfigureCabal ::
     FilePath -- ^ The @.cabal@ file.
  -> [String] -- ^ Arguments to @cabal configure@
  -> FilePath -- ^ Build directory (e.g., @./dist/@)
  -> IO (C.LocalBuildInfo, TimeStamp)
maybeConfigureCabal cabal_file config_flags odir = do
  lbi_or_err <- try $ C.getPersistBuildConfig odir
  case lbi_or_err of
    Left (_e :: IOException) -> conf
    Right lbi -> do
      is_old <- checkPersistBuildConfigOutdated odir cabal_file
      if is_old then conf else do
        t <- getModificationTime (C.localBuildInfoFile odir)
        debugMsg "Project already configured"
        return (lbi, t)
 where
   conf = configureCabal cabal_file config_flags odir

checkPersistBuildConfigOutdated :: FilePath -> FilePath -> IO Bool
-- #if MIN_VERSION_Cabal(1,10,0)
-- checkPersistBuildConfigOutdated = C.checkPersistBuildConfigOutdated
-- #else
checkPersistBuildConfigOutdated distPref pkg_descr_file = do
  t0 <- getModificationTime pkg_descr_file
  t1 <- getModificationTime $ C.localBuildInfoFile distPref
  return (t0 > t1)
-- #endif

-- TODO: Move into separate module.  Scion.FileUtils maybe?
getModificationTime :: FilePath -> IO TimeStamp
getModificationTime file =
  convert . modificationTime <$> getFileStatus file

-- | Make sure the given Cabal project is configured.
--
-- TODO: handle failure
configureCabal :: FilePath -- ^ The @.cabal@ file.
               -> [String] -- ^ Arguments to @cabal configure@
               -> FilePath
               -> IO (C.LocalBuildInfo, TimeStamp)
configureCabal cabal_file0 config_flags odir = do
  dir0 <- getCurrentDirectory

  -- 1. Make sure the .cabal file is an absolute path name.
  let cabal_file = dir0 </> cabal_file0

  -- 2. Cabal typically assumes to be in the same directory as the
  -- .cabal file
  setCurrentDirectory (dropFileName cabal_file)

  debugMsg $ "Configuring Cabal project: " ++ show cabal_file

  -- 3. Convince Cabal to parse a @configure ...stuff..@ command line.
  gen_pkg_descr <- C.readPackageDescription C.normal cabal_file
  cf0 <- case C.commandsRun confCmd commands config_flags of
           C.CommandReadyToGo (flags_, _args) -> return flags_
           -- TODO: Better error messages.
           _ -> throwIO $ userError "Could not parse config flags."

  --debugMsg $ "GHC: " ++ show ghc ++ " " ++ show ghc_pkg
  
  -- 4. Now we do Cabal's configuration step.
  -- TODO: We should probably specify the version of GHC more tightly.
  let conf_flags =
        cf0{ C.configDistPref = C.toFlag odir,
             -- Make sure we use the exact same GHC version that we
             -- linked against
             C.configHcPath = C.toFlag ghc,
             C.configHcPkg = C.toFlag ghc_pkg,
             C.configHcFlavor = C.toFlag C.GHC,
             C.configUserInstall = C.toFlag True
           }
  debugMsg $ "ConfigFlags: " ++ show conf_flags

  lcl_build_info <- C.configure (gen_pkg_descr, C.emptyHookedBuildInfo)
                                conf_flags

  -- 5. Always write the result
  C.writePersistBuildConfig odir lcl_build_info

  C.initialBuildSteps odir (C.localPkgDescr lcl_build_info) lcl_build_info
#if __GLASGOW_HASKELL__ >= 702
                      C.normal
#else
                      C.normal C.knownSuffixHandlers
#endif

  -- Create timestamp *after* writing the file.  Thus if we later
  -- check if the file is up to date using this timestamp, it is
  -- considered up to date.  (Can this happen?)
  --
  -- TODO: It may be more robust to get the timestamp from the file.
  timestamp <- convert <$> getCurrentTime
  -- 6. Revert back to the original directory
  setCurrentDirectory dir0

  return (lcl_build_info, timestamp)
 where
   confCmd :: C.CommandUI C.ConfigFlags
   confCmd = C.configureCommand C.defaultProgramConfiguration

   commands = [ confCmd `C.commandAddAction` \fs as -> (fs, as) ]


getComponentInfo :: C.LocalBuildInfo -> Component
                  -> Maybe (Either C.Library C.Executable,
                            C.ComponentLocalBuildInfo,
                            C.BuildInfo)
getComponentInfo lbi Library =
  let lib = C.library (C.localPkgDescr lbi) in
  (,,) <$> (Left <$> lib)
       <*> C.libraryConfig lbi
       <*> (C.libBuildInfo <$> lib)

getComponentInfo lbi (Executable exename) =
  (,,) <$> (Right <$> exe) <*> exe_clbi <*> exe_bi
 where
   exe_clbi =
     snd <$> find ((==exename) . fst) (C.executableConfigs lbi)
   exe = find ((==exename) . C.exeName)
              (C.executables (C.localPkgDescr lbi))
   exe_bi = C.buildInfo <$> exe


-- -------------------------------------------------------------------

soloWorkerMain :: IO ()
soloWorkerMain = do
  -- 1. We will use stdin/stdout to communicate with the server.
  -- stderr will be used for logging.
  let inp = stdin
  --out <- makeExclusive stdout stderr
  let out = stdout
  mapM_ ensureBinaryMode [inp] --, out]
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  hFlush stderr

  hPutStrLn stdout "test"
  debugMsg "=== Starting worker =============================="

  -- The arguments are the working directory and GHC flags.
  let args = ["tests/projects/"]
--  args <- getArgs
  debugMsg $ "Args: " ++ show args
  let worker_dir:_other_args = args
  setCurrentDirectory worker_dir
  debugMsg $ "Worker dir: " ++ worker_dir

--  debugMsg "Sending READY"
--  C.hPut out (C.pack "READY\n")
--  hFlush out

  debugMsg "Receiving SessionConfig ..."

--  msg0 <- recvMessageFromHandle inp
--  case (msg0 :: Maybe SessionConfig) of
--    Just sess_conf -> do
  let sess_conf = FileConfig {sc_fileName = "tests/projects/file001.hs", sc_flags = []}
  debugMsg $ "OK: " ++ show sess_conf

  initWorker sess_conf debugMsg (main_loop inp out)
  return ()

{-
    Nothing -> do
      debugMsg "ERROR"
      return ()
-}
