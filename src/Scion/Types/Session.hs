{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, DeriveDataTypeable,
             MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Scion.Types.Session
  ( module Scion.Types.Session
  , module Scion.Types.Core
  )
where

import           Scion.Utils.Convert
import           Scion.Types.Note
import           Scion.Types.Core

import           Paths_scion as Info ( getBinDir )

import           Control.Applicative
import           Data.Binary
import           Data.List ( intercalate )
import           Data.Monoid
import qualified Data.MultiSet as MS
import           Data.String ( IsString(fromString) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock ( UTCTime, NominalDiffTime )
import           Data.Time.Format ( formatTime )
import           Data.Typeable ( Typeable )
import qualified Distribution.ModuleName as DM
import           System.Directory ( findExecutable, doesFileExist )
import           System.FilePath ( (</>) )
import           System.FilePath.Canonical
import           System.IO ( Handle )
import           System.Locale ( defaultTimeLocale )
import           System.Process ( ProcessHandle, runInteractiveProcess )
import           Foreign.C.Types ( CTime )
import           Data.Time.Clock.POSIX ( posixSecondsToUTCTime )

newtype SessionId = SessionId Int
  deriving (Eq, Ord, Enum)

instance Show SessionId where
  show (SessionId sid) = "sid" ++ show sid

firstSessionId :: SessionId
firstSessionId = SessionId 1

unsafeSessionIdToInt :: SessionId -> Int
unsafeSessionIdToInt (SessionId n) = n

unsafeSessionIdFromInt :: Int -> SessionId
unsafeSessionIdFromInt = SessionId

data Component = Library | Executable String
  deriving (Eq,Ord,Show)
instance Binary Component where
  put Library = putWord8 1
  put (Executable s) = putWord8 2 >> put s
  get = do tag <- getWord8
           case tag of
             1 -> return Library
             2 -> Executable <$> get
             _ -> fail "Binary Component get: tag error"

-- | A @WorkerHandle@ contains the state and data structures for
-- communicating with a worker process.
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

-- | A @SessionConfig@ describes how a session is to be initialised.
--
-- In particular, GHC needs to know about the root modules and static
-- and dynamic flags.
data SessionConfig =
  -- | A single file and command line flags.
  FileConfig
    { sc_fileName :: FilePath
    , sc_flags    :: [String]
      -- ^ Command line flags that would be passed to GHC.
    }
  |
  -- | A configuration based on a @.cabal@ file.
  CabalConfig
    { sc_name :: String
      -- ^ A name for this configuration.  This is presented to the
      -- user, e.g., \"release\", or \"testing\".
    , sc_cabalFile :: FilePath
      -- ^ The @.cabal@ file describing the project.  This file must
      -- be located in the root path of the project.
    , sc_component :: Component
      -- ^ The library (@Nothing@) or an executable (@Just exeName@).
    , sc_configFlags :: [String]
      -- ^ Flags that would be passed to @cabal configure@.
    , sc_buildDir :: Maybe FilePath
      -- ^ The directory where temporary and binary files are put.  If
      -- @Nothing@, a temporary directory will be chosen.
    }
  |
  -- | A configuration with no files.
  EmptyConfig
    { sc_flags :: [String] }
  deriving (Show)

instance Eq SessionConfig where
  c1@FileConfig{} == c2@FileConfig{} =
    sc_fileName c1 == sc_fileName c2 && sc_flags c1 == sc_flags c2
  c1@CabalConfig{} == c2@CabalConfig{} =
    sc_name c1 == sc_name c2 &&
    sc_cabalFile c1 == sc_cabalFile c2 &&
    sc_component c1 == sc_component c2 &&
    sc_configFlags c1 == sc_configFlags c2
    -- Ignore buildDir when testing for equality
  c1@EmptyConfig{} == c2@EmptyConfig{} = sc_flags c1 == sc_flags c2
  _ == _ = False

-- | The @SessionState@ contains the cached part of a worker's state.
data SessionState = SessionState
  { sessionConfig :: SessionConfig
  , sessionConfigTimeStamp :: TimeStamp
    -- ^ The timestamp of the session config.
    --
    -- For a 'FileConfig' this is the modification date of the file;
    -- for a 'CabalConfig' it is the modification date of the @.cabal@
    -- file.
  , sessionWorker :: WorkerHandle
  , sessionOutputDir :: FilePath
    -- ^ Use this directory for storing any stuff on disk.  Due to
    -- garbage collection we cannot easily take advantage of virtual
    -- memory.  Instead we write most things to disk but rely on I\/O
    -- caches to speed things up.
  , sessionModuleGraph :: [ModuleSummary]
  , sessionLastCompilation :: CompilationResult
  , sessionHomeDir :: CanonicalFilePath
    -- ^ All file paths are relative to this directory.
  } deriving (Show)

instance Binary SessionConfig where
  put (FileConfig f fs) =
    putWord8 1 >> put f >> put fs
  put (CabalConfig nm fp comp flags odir) =
    putWord8 2 >> put nm >> put fp >> put comp >> put flags >> put odir
  put (EmptyConfig fs) =
    putWord8 3 >> put fs
  get = do tag <- getWord8
           case tag of
             1 -> FileConfig <$> get <*> get
             2 -> CabalConfig <$> get <*> get <*> get <*> get <*> get
             3 -> EmptyConfig <$> get
             _ -> fail "Binary SessionConfig get: tag error"


-- | The concept of \"a point in time\" that we use throughout Scion.
newtype TimeStamp = TimeStamp { timeStampUTCTime :: UTCTime }
  deriving (Eq, Ord)

instance Convert CTime TimeStamp where
  convert epoch =
    TimeStamp . posixSecondsToUTCTime . realToFrac $ epoch

instance Convert UTCTime TimeStamp where
  convert = TimeStamp

instance Show TimeStamp where
  show (TimeStamp t) =
    formatTime defaultTimeLocale "%Y-%m-%d-%T" t

-- | Function that starts a worker.  The arguments are:
--
--  1. The working directory of the worker.
--
--  2. The command line arguments to initialise the GHC API.
--
-- The results are the same as for 'System.Process.runInteractiveProcess'.
type WorkerStarter =
  FilePath -> [String] -> IO (Handle, Handle, Handle, ProcessHandle)

defaultWorkerStarter :: String -> WorkerStarter
defaultWorkerStarter workername homedir args = do
  worker <- do
    bindir <- Info.getBinDir
    has_inplace <- doesFileExist (bindir </> workername)
    if has_inplace then return (bindir </> workername)
     else do
       mb_worker <- findExecutable workername
       case mb_worker of
         Nothing ->
           throwIO $ CannotStartWorker $
             "Executable \"" ++ workername ++ "\" does not exist"
         Just w -> return w
  --putStrLn $ "Starting worker process: " ++ worker
  runInteractiveProcess worker (homedir:args) Nothing Nothing

data CannotStartWorker = CannotStartWorker String
  deriving (Show, Typeable)

instance Exception CannotStartWorker

-- | Scion's own concept of a module name.  (Convertible to and from
-- GHC's and Cabal's versions.)
newtype ModuleName = ModuleName T.Text
  deriving (Eq, Ord)

moduleNametoText :: ModuleName -> T.Text
moduleNametoText (ModuleName t) = t

instance IsString ModuleName where
  fromString = ModuleName . fromString

instance Show ModuleName where
  show (ModuleName n) = T.unpack n

instance Binary ModuleName where
  put (ModuleName mn) = put (T.encodeUtf8 mn)
  get = ModuleName . T.decodeUtf8 <$> get

instance Convert DM.ModuleName ModuleName where
  convert m = fromString (intercalate "." (DM.components m))

-- | A summary of a module.
--
-- This contains top-level information such as module name and
-- dependencies.
data ModuleSummary = ModuleSummary 
  { ms_module   :: ModuleName
  , ms_fileType :: HsFileType
  , ms_imports  :: [ModuleName]
  , ms_location :: CanonicalFilePath
  } deriving Eq

instance Show ModuleSummary where
  show ms =
    "<summary:" ++ show (ms_module ms) ++ "," ++
    show (ms_location ms) ++ ">"

instance Binary ModuleSummary where
  put (ModuleSummary mdl ft imps loc) =
    put mdl >> put ft >> put imps >> put loc
  get = ModuleSummary <$> get <*> get <*> get <*> get

instance Binary CanonicalFilePath where
  put cfp = put (originalFilePath cfp) >> put (canonicalFilePath cfp)
  get = unsafeCanonicalise <$> get <*> get

data HsFileType 
  = HaskellFile
  | HaskellBootFile
  | ExternalCoreFile
  deriving (Eq, Ord, Show, Enum)

instance Binary HsFileType where
  put ft = putWord8 (fromIntegral (fromEnum ft))
  get = toEnum . fromIntegral <$> getWord8

-- -------------------------------------------------------------------

data CompilationResult = CompilationResult { 
      compilationSucceeded :: Bool,
      compilationNotes     :: MS.MultiSet Note,
      compilationTime      :: NominalDiffTime
    } deriving Show

instance Monoid CompilationResult where
  mempty = CompilationResult True mempty 0
  mappend r1 r2 =
      CompilationResult 
        { compilationSucceeded = 
              compilationSucceeded r1 && compilationSucceeded r2
        , compilationNotes =
            compilationNotes r1 `MS.union` compilationNotes r2
        , compilationTime = compilationTime r1 + compilationTime r2
        }

instance Binary CompilationResult where
  put (CompilationResult ok notes time) =
    put ok >> put (MS.toAscList notes) >> putNominalDiffTime time
  get = CompilationResult <$> get 
                          <*> (MS.fromAscList <$> get) 
                          <*> getNominalDiffTime

putNominalDiffTime :: NominalDiffTime -> Put
putNominalDiffTime t = put (toRational t)

getNominalDiffTime :: Get NominalDiffTime
getNominalDiffTime = fromRational <$> get

data Target
  = ModuleTarget ModuleName
  | FileTarget FilePath
  | CabalTarget FilePath
  deriving (Eq, Ord, Show)

instance Binary Target where
  put (ModuleTarget mn) = putWord8 1 >> put mn
  put (FileTarget fp)   = putWord8 2 >> put fp
  put (CabalTarget fp)  = putWord8 3 >> put fp
  get = do tag <- getWord8
           case tag of
             1 -> ModuleTarget <$> get
             2 -> FileTarget <$> get
             3 -> CabalTarget <$> get
             _ -> fail "Binary Target get: tag error"
