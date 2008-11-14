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
-- Core types used throughout Scion. 
--
module Scion.Types 
  ( module Scion.Types
  , liftIO, MonadIO
  ) where

import GHC
import HscTypes
import MonadUtils ( liftIO, MonadIO )
import Exception
import qualified GHC

import Distribution.Simple.LocalBuildInfo
import Control.Monad ( when )
import Data.IORef
import Data.Typeable
import Control.Exception

data SessionState 
  = SessionState {
      scionVerbosity :: Verbosity,
      initialDynFlags :: DynFlags,
        -- ^ The DynFlags as they when Scion was started.  This is used to
        -- reset flags when opening a new project.  Arguably, the GHC API
        -- should provide calls to reset a session.

      localBuildInfo :: Maybe LocalBuildInfo,
        -- ^ Build info from current Cabal project.

      activeComponent :: Maybe CabalComponent,
        -- ^ The current active Cabal component.  This affects DynFlags and
        -- targets.  ATM, we don't support multiple active components.

      focusedModule :: Maybe (FilePath, ModuleName, ModSummary)
        -- ^ The currently focused module for background typechecking.
    }

mkSessionState :: DynFlags -> IO (IORef SessionState)
mkSessionState dflags =
    newIORef (SessionState normal dflags Nothing Nothing Nothing)

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

data Verbosity
  = Silent
  | Normal
  | Verbose
  | Deafening
  deriving (Eq, Ord, Show, Enum, Bounded)

silent :: Verbosity
silent = Silent

normal :: Verbosity
normal = Normal

verbose :: Verbosity
verbose = Verbose

deafening :: Verbosity
deafening = Deafening

message :: Verbosity -> String -> ScionM ()
message v s = do
  v0 <- gets scionVerbosity
  when (v0 >= v) $ liftIO $ putStrLn s

-- | Reflect a computation in the 'ScionM' monad into the 'IO' monad.
reflectScionM :: ScionM a -> (IORef SessionState, Session) -> IO a
reflectScionM (ScionM f) = \(st, sess) -> reflectGhc (f st) sess

-- > Dual to 'reflectGhc'.  See its documentation.
reifyScionM :: ((IORef SessionState, Session) -> IO a) -> ScionM a
reifyScionM act = ScionM $ \st -> reifyGhc $ \sess -> act (st, sess)

------------------------------------------------------------------------------

data CabalComponent = Library | Executable String deriving (Eq, Show, Typeable)

------------------------------------------------------------------------------

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

dieHard :: String -> a
dieHard last_wish = do
   error $ "************** Panic **************\n" ++ 
              last_wish ++ 
              "\nPlease file a bug report at:\n  " ++ bug_tracker_url
  where
    bug_tracker_url = "http://code.google.com/p/scion-lib/issues/list"

