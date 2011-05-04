-- | The types used by the worker (which talks to the GHC API.)
module Scion.Types.Worker
  ( module Scion.Types.Worker
  , module Scion.Types.Core
  , MonadIO(..), ExceptionMonad(..) )
where

import Scion.Types.Core
import Scion.Types.Note ( Note )

import Control.Applicative
import Data.IORef
import System.IO
import Distribution.Simple.LocalBuildInfo
import GHC       ( Ghc, GhcMonad(..) )
import HscTypes  ( WarnLogMonad(..) )
import MonadUtils ( MonadIO, liftIO )
import Exception ( ExceptionMonad(..) )

newtype Worker a
  = Worker { unWorker :: IORef WorkerState -> Ghc a }

data WorkerState = WorkerState
  { workerLBI       :: Maybe LocalBuildInfo
  , workerLogHandle :: Maybe Handle
  , workerLogLevel  :: Int
  , workerNewNotes  :: IORef [Note]
  }

mkWorkerState :: IORef [Note] -> IO (IORef WorkerState)
mkWorkerState r = newIORef $ WorkerState
  { workerLBI = Nothing
  , workerLogHandle = Nothing
  , workerLogLevel = 0
  , workerNewNotes = r}

instance Functor Worker where
  fmap f (Worker g) = Worker $ \r -> fmap f (g r)

instance Applicative Worker where
  pure x = Worker $ \_ -> return x
  Worker af <*> Worker ax =
    Worker $ \r -> do f <- af r; x <- ax r; return (f x)

instance Monad Worker where
  return x = pure x
  Worker f >>= k = Worker $ \r -> do
    a <- f r
    unWorker (k a) r

instance MonadIO Worker where
  liftIO io = Worker $ \_ -> liftIO io

instance ExceptionMonad Worker where
  gcatch (Worker act) handler =
    Worker $ \r -> act r `gcatch` (\e -> unWorker (handler e) r)
  gblock (Worker act) = Worker $ \r -> gblock (act r)
  gunblock (Worker act) = Worker $ \r -> gunblock (act r)

instance WarnLogMonad Worker where
  setWarnings ws = Worker $ \_ -> setWarnings ws
  getWarnings = Worker $ \_ -> getWarnings

instance GhcMonad Worker where
  getSession = Worker (\_ -> getSession)
  setSession s = Worker (\_ -> setSession s)

getAndClearNewNotes :: Worker [Note]
getAndClearNewNotes = Worker $ \r -> liftIO $ do
  nn <- workerNewNotes <$> readIORef r
  atomicModifyIORef nn $ \ns -> ([], ns)

newtype Verbosity = Verbosity Int
  deriving (Eq, Ord)

silent :: Verbosity
silent = Verbosity 0

normal :: Verbosity
normal = Verbosity 1

verbose :: Verbosity
verbose = Verbosity 2

deafening :: Verbosity
deafening = Verbosity 3

message :: Verbosity -> String -> Worker ()
message _ msg = io $ hPutStrLn stderr msg
