{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
-- |
-- Module      : Scion.Types
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Types used throughout Scion. 
--
module Scion.Types 
  ( module Scion.Types
  , liftIO, MonadIO
  ) where

import Scion.Types.ExtraInstances()

import GHC
import ErrUtils ( WarningMessages, ErrorMessages )
import HscTypes
import MonadUtils ( liftIO, MonadIO )
import Exception

import Distribution.Simple.LocalBuildInfo
import Control.Monad ( when )
import Data.IORef
import Data.Monoid
import Data.Time.Clock  ( NominalDiffTime )
import Data.Typeable
import Control.Exception
import Control.Applicative

------------------------------------------------------------------------------
-- * The Scion Monad and Session State

-- XXX: Can we get rid of some of this maybe stuff?
data SessionState 
  = SessionState {
      scionVerbosity :: Verbosity,
      initialDynFlags :: DynFlags,
        -- ^ The DynFlags as they were when Scion was started.  This is used
        -- to reset flags when opening a new project.  Arguably, the GHC API
        -- should provide calls to reset a session.

      localBuildInfo :: Maybe LocalBuildInfo,
        -- ^ Build info from current Cabal project.

      activeComponent :: Maybe Component,
        -- ^ The current active Cabal component.  This affects DynFlags and
        -- targets.  ATM, we don't support multiple active components.

      lastCompResult :: CompilationResult,

      focusedModule :: Maybe ModSummary,
        -- ^ The currently focused module for background typechecking.

      bgTcCache :: Maybe BgTcCache
        -- ^ Cached state of the background typechecker.
    }

mkSessionState :: DynFlags -> IO (IORef SessionState)
mkSessionState dflags =
    newIORef (SessionState normal dflags Nothing Nothing mempty Nothing Nothing)


newtype ScionM a
  = ScionM { unScionM :: IORef SessionState -> Ghc a }

instance Monad ScionM where
  return x = ScionM $ \_ -> return x
  (ScionM ma) >>= fb = 
      ScionM $ \s -> do
        a <- ma s 
        unScionM (fb a) s
  fail msg = dieHard msg

instance Functor ScionM where
  fmap f (ScionM ma) =
      ScionM $ \s -> fmap f (ma s)

instance Applicative ScionM where
  pure a = ScionM $ \_ -> return a
  ScionM mf <*> ScionM ma = 
      ScionM $ \s -> do f <- mf s; a <- ma s; return (f a)

liftScionM :: Ghc a -> ScionM a
liftScionM m = ScionM $ \_ -> m

instance MonadIO ScionM where
  liftIO m = liftScionM $ liftIO m

instance ExceptionMonad ScionM where
  gcatch (ScionM act) handler =
      ScionM $ \s -> act s `gcatch` (\e -> unScionM (handler e) s)
  gblock (ScionM act) = ScionM $ \s -> gblock (act s)
  gunblock (ScionM act) = ScionM $ \s -> gunblock (act s)

instance WarnLogMonad ScionM where
  setWarnings = liftScionM . setWarnings
  getWarnings = liftScionM getWarnings

instance GhcMonad ScionM where
  getSession = liftScionM getSession
  setSession = liftScionM . setSession

modifySessionState :: (SessionState -> SessionState) -> ScionM ()
modifySessionState f =
    ScionM $ \r -> liftIO $ do s <- readIORef r; writeIORef r $! f s

getSessionState :: ScionM SessionState
getSessionState = ScionM $ \s -> liftIO $ readIORef s

gets :: (SessionState -> a) -> ScionM a
gets sel = getSessionState >>= return . sel

setSessionState :: SessionState -> ScionM ()
setSessionState s' = ScionM $ \r -> liftIO $ writeIORef r s'

------------------------------------------------------------------------------
-- ** Verbosity Levels

data Verbosity
  = Silent
  | Normal
  | Verbose
  | Deafening
  deriving (Eq, Ord, Show, Enum, Bounded)

intToVerbosity :: Int -> Verbosity
intToVerbosity n
  | n < 0                                = minBound
  | n > fromEnum (maxBound :: Verbosity) = maxBound
  | otherwise                            = toEnum n

verbosityToInt :: Verbosity -> Int
verbosityToInt = fromEnum

silent :: Verbosity
silent = Silent

normal :: Verbosity
normal = Normal

verbose :: Verbosity
verbose = Verbose

deafening :: Verbosity
deafening = Deafening

getVerbosity :: ScionM Verbosity
getVerbosity = gets scionVerbosity

setVerbosity :: Verbosity -> ScionM ()
setVerbosity v = modifySessionState $ \s -> s { scionVerbosity = v }

message :: Verbosity -> String -> ScionM ()
message v s = do
  v0 <- getVerbosity
  when (v0 >= v) $ liftIO $ putStrLn s

------------------------------------------------------------------------
-- * Reflection into IO

-- | Reflect a computation in the 'ScionM' monad into the 'IO' monad.
reflectScionM :: ScionM a -> (IORef SessionState, Session) -> IO a
reflectScionM (ScionM f) = \(st, sess) -> reflectGhc (f st) sess

-- | Dual to 'reflectScionM'.  See its documentation.
reifyScionM :: ((IORef SessionState, Session) -> IO a) -> ScionM a
reifyScionM act = ScionM $ \st -> reifyGhc $ \sess -> act (st, sess)

------------------------------------------------------------------------------
-- * Compilation Results

data BgTcCache
  = Parsed ParsedModule
  | Typechecked TypecheckedModule

data CompilationResult = CompilationResult { 
      compilationSucceeded :: Bool,
      compilationWarnings  :: WarningMessages,
      compilationErrors    :: ErrorMessages,
      compilationTime      :: NominalDiffTime
    }

instance Monoid CompilationResult where
  mempty = CompilationResult True mempty mempty 0
  mappend r1 r2 =
      CompilationResult 
        { compilationSucceeded = 
              compilationSucceeded r1 && compilationSucceeded r2
        , compilationWarnings = 
            compilationWarnings r1 `mappend` compilationWarnings r2
        , compilationErrors =
            compilationErrors r1 `mappend` compilationErrors r2
        , compilationTime = compilationTime r1 + compilationTime r2
        }

------------------------------------------------------------------------------
-- * Exceptions

-- | Any exception raised inside Scion is a subtype of this exception.
data SomeScionException
  = forall e. (Exception e) => SomeScionException e
  deriving Typeable

instance Show SomeScionException where show (SomeScionException e) = show e
instance Exception SomeScionException

scionToException :: Exception e => e -> SomeException
scionToException = toException . SomeScionException

scionFromException :: Exception e => SomeException -> Maybe e
scionFromException x = do
  SomeScionException e <- fromException x
  cast e

-- | A fatal error.  Like 'error' but suggests submitting a bug report.
dieHard :: String -> a
dieHard last_wish = do
   error $ "************** Panic **************\n" ++ 
              last_wish ++ 
              "\nPlease file a bug report at:\n  " ++ bug_tracker_url
  where
    bug_tracker_url = "http://code.google.com/p/scion-lib/issues/list"

------------------------------------------------------------------------------
-- * Others \/ Helpers

data Component 
  = Library
  | Executable String
  | File FilePath
  deriving (Eq, Show, Typeable)

-- | Shorthand for 'undefined'.
__ :: a
__ = undefined
