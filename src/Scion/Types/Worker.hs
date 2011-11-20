{-# LANGUAGE CPP #-}
-- | The types used by the worker (which talks to the GHC API.)
module Scion.Types.Worker
  ( module Scion.Types.Worker
  , module Scion.Types.Core
  )
where

import Scion.Types.Core
import Scion.Types.Note ( Note )

import Control.Applicative
import Control.Monad ( when )
import Data.IORef
import System.IO
import Distribution.Simple.LocalBuildInfo
import GHC       ( Ghc, GhcMonad(..) )
#if __GLASGOW_HASKELL__ < 702
import HscTypes  ( WarnLogMonad(..) )
import MonadUtils ( MonadIO, liftIO )
import Exception ( ExceptionMonad(..) )
#endif

newtype Worker a
  = Worker { unWorker :: IORef WorkerState -> Ghc a }

data WorkerState = WorkerState
  { workerLBI       :: Maybe LocalBuildInfo
  , workerLogHandle :: Maybe Handle
  , workerLogLevel  :: Verbosity
  , workerNewNotes  :: IORef [Note]
  }

mkWorkerState :: IORef [Note] -> IO (IORef WorkerState)
mkWorkerState r = newIORef $ WorkerState
  { workerLBI = Nothing
  , workerLogHandle = Nothing
  , workerLogLevel = normal
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
  liftIO act = Worker $ \_ -> liftIO act

instance ExceptionMonad Worker where
  gcatch (Worker act) handler =
    Worker $ \r -> act r `gcatch` (\e -> unWorker (handler e) r)
  gblock (Worker act) = Worker $ \r -> gblock (act r)
  gunblock (Worker act) = Worker $ \r -> gunblock (act r)

#if __GLASGOW_HASKELL__ < 702
instance WarnLogMonad Worker where
  setWarnings ws = Worker $ \_ -> setWarnings ws
  getWarnings = Worker $ \_ -> getWarnings
#endif

instance GhcMonad Worker where
  getSession = Worker (\_ -> getSession)
  setSession s = Worker (\_ -> setSession s)

getAndClearNewNotes :: Worker [Note]
getAndClearNewNotes = Worker $ \r -> liftIO $ do
  nn <- workerNewNotes <$> readIORef r
  atomicModifyIORef nn $ \ns -> ([], ns)

instance LogMonad Worker where
  setVerbosity v = Worker $ \r ->
    io (atomicModifyIORef r (\ws -> (ws{ workerLogLevel = v }, ())))
  getVerbosity = Worker $ \r -> workerLogLevel <$> io (readIORef r)
  message verb msg = do
    v <- getVerbosity
    when (verb <= v) $ io $ hPutStrLn stderr msg >> hFlush stderr
