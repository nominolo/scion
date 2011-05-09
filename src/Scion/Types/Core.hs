{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}
module Scion.Types.Core (
  -- * Generalised IO and Exception monads
  module Exception,
  MonadIO(..),
  io, gcatches, HandlerM(..),
  -- * Exception Types 
  ScionException(..), scionError, 
  -- * Logging
  Verbosity, silent, normal, verbose, deafening,
  LogMonad(..),
) where

import MonadUtils ( MonadIO(..) ) -- from GHC
import Exception
import Data.Typeable ( Typeable )

-- | A short name for 'liftIO'.
io :: MonadIO m => IO a -> m a
io = liftIO
{-# INLINE io #-}

-- | A generalised version of 'Control.Exception.catches'.
--
-- Example use:
-- 
-- > f = expr `gcatches` [HandlerM (\(ex :: ArithException) -> handleArith ex),
-- >                      HandlerM (\(ex :: IOException)    -> handleIO    ex)]
--
gcatches :: (MonadIO m, ExceptionMonad m) =>
            m a -> [HandlerM m a] -> m a
gcatches act handlers = act `gcatch` catchesHandler handlers

-- | You need this when using 'catches'.
data HandlerM m a = forall e . Exception e => HandlerM (e -> m a)

catchesHandler :: (MonadIO m, ExceptionMonad m) => 
                  [HandlerM m a] -> SomeException -> m a
catchesHandler handlers e = foldr tryHandler (io (throwIO e)) handlers
    where tryHandler (HandlerM handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res

data ScionException = ScionException String
  deriving (Typeable)

instance Show ScionException where
  show (ScionException msg) = "Scion Exception: " ++ msg

instance Exception ScionException

-- | Utility function for throwing a 'ScionException'.  This uses
-- 'throwIO' under the hood which ensures that exceptions are properly
-- serialised with the underlying monad effects.
scionError :: MonadIO m => String -> m a
scionError msg = io $ throwIO $ ScionException msg

-- | Describes the verbosity level for logging.
newtype Verbosity = Verbosity Int
  deriving (Eq, Ord)

-- | Minimal verbosity.
silent :: Verbosity
silent = Verbosity 0

-- | Default verbosity.
normal :: Verbosity
normal = Verbosity 1

-- | Increased verbosity.
verbose :: Verbosity
verbose = Verbosity 2

-- | Maximum verbosity.
deafening :: Verbosity
deafening = Verbosity 3

-- | A class for monads that support logging.
class LogMonad m where
  -- | Sent a message with the given verbosity.
  --
  -- A @message v msg@ is printed if the configured verbosity is @>= v@.
  message :: Verbosity -> String -> m ()
  
  -- | Set verbosity level.
  setVerbosity :: Verbosity -> m ()

  -- | Get current verbosity level.
  getVerbosity :: m Verbosity
