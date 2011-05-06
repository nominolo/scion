{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns #-}
-- | Definitions concerning the
module Scion.Types.Monad
  ( module Scion.Types.Monad,
    module Scion.Types.Core,
    ExceptionMonad(..), MonadIO(..)
  )
where

import           Scion.Types.Compiler
import           Scion.Types.Session
import           Scion.Types.Core

import           Control.Applicative
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.IORef
import           MonadUtils -- from GHC
import           Exception  -- from GHC

-- * The Scion Monad and Session State

data GlobalState = GlobalState
  { gsSessions :: M.Map SessionId SessionState
  , gsNextSessionId :: !SessionId
  , gsWorkerStarter :: WorkerStarter
  , gsLogLevel :: Int
  , gsExtensions :: Maybe [Extension]
  }

mkGlobalState :: IO (IORef GlobalState)
mkGlobalState = newIORef
  GlobalState { gsSessions = M.empty
              , gsNextSessionId = firstSessionId
              , gsWorkerStarter = defaultWorkerStarter "scion-worker"
              , gsLogLevel = 0
              , gsExtensions = Nothing
              }

-- | The 'ScionM' monad.  It contains the state to manage multiple
-- active sessions.
newtype ScionM a
  = ScionM { unScionM :: IORef GlobalState -> IO a }

runScion :: ScionM a -> IO a
runScion m = do
  ref <- mkGlobalState
  unScionM m ref

instance Monad ScionM where
  return x = ScionM $ \_ -> return x
  (ScionM ma) >>= fb = ScionM $ \s -> do
                         a <- ma s
                         unScionM (fb a) s
  fail msg = ScionM $ \_ -> throwIO $ ScionException $ "FATAL: " ++ msg

instance Functor ScionM where
  fmap f (ScionM ma) = ScionM (fmap f . ma)

instance Applicative ScionM where
  pure a = ScionM $ \_ -> return a
  ScionM mf <*> ScionM ma =
      ScionM $ \s -> do f <- mf s; a <- ma s; return (f a)

liftScionM :: IO a -> ScionM a
liftScionM m = ScionM $ \_ -> m

getLogLevel :: ScionM Int
getLogLevel = ScionM $ \r -> gsLogLevel <$> readIORef r

genSessionId :: ScionM SessionId
genSessionId = ScionM $ \ref ->
  atomicModifyIORef ref $ \gs ->
    let !sid = gsNextSessionId gs in
    (gs{ gsNextSessionId = succ sid }, sid)

-- | Register a 'SessionState' with the given 'SessionId'. (Internal)
--
-- Assumes that no other state is registered with this @SessionId@.
registerSession :: SessionId -> SessionState -> ScionM ()
registerSession sid sess = ScionM $ \r ->
  atomicModifyIORef r $ \gs ->
    let !sessions' = M.insert sid sess (gsSessions gs) in
    (gs{ gsSessions = sessions' }, ())

-- | Return the state for the 'SessionId'.  The session must exist.
getSessionState :: SessionId -> ScionM SessionState
getSessionState sid = ScionM $ \r -> do
  gs <- readIORef r
  case M.lookup sid (gsSessions gs) of
    Just s -> return s
    Nothing -> error $ "Not an active session: " ++ show sid

-- | Unregister a 'SessionId'.  NOTE: Does not stop the worker.
unregisterSession :: SessionId -> ScionM ()
unregisterSession sid = ScionM $ \r ->
  atomicModifyIORef r $ \gs ->
    let !sessions' = M.delete sid (gsSessions gs) in
    let !gs' = gs{ gsSessions = sessions' } in
    (gs', ())

-- | Set the function that starts a worker process.  See
-- 'WorkerStarter'.
setWorkerStarter :: WorkerStarter -> ScionM ()
setWorkerStarter f = ScionM $ \r ->
  atomicModifyIORef r $ \gs -> (gs{ gsWorkerStarter = f }, ())

-- | Get the current function that starts a worker process.  See
-- 'WorkerStarter'.
getWorkerStarter :: ScionM WorkerStarter
getWorkerStarter =
  ScionM $ \r -> gsWorkerStarter `fmap` readIORef r

modifySessionState :: SessionId -> (SessionState -> (SessionState, a))
                   ->  ScionM a
modifySessionState sid f = ScionM $ \r ->
  atomicModifyIORef r $ \gs ->
    case M.lookup sid (gsSessions gs) of
      Just ss -> do
        let (!ss', a) = f ss
        (gs{ gsSessions = M.insert sid ss' (gsSessions gs) }, a)
      Nothing ->
        error $ "modifySessionState: Not an active session: " ++ show sid

getExtensions :: ScionM (Maybe [Extension])
getExtensions = ScionM $ \r -> gsExtensions <$> readIORef r

setExtensions :: [Extension] -> ScionM ()
setExtensions exts = ScionM $ \r ->
  atomicModifyIORef r $ \gs ->
    (gs{ gsExtensions = Just exts }, ())

instance MonadIO ScionM where
  liftIO m = liftScionM $ liftIO m

instance ExceptionMonad ScionM where
  gcatch (ScionM act) handler =
      ScionM $ \s -> act s `gcatch` (\e -> unScionM (handler e) s)
  gblock (ScionM act) = ScionM $ \s -> gblock (act s)
  gunblock (ScionM act) = ScionM $ \s -> gunblock (act s)

