module Scion.Types where

import GHC
import HscTypes
import MonadUtils ( liftIO, MonadIO )
import Exception
import qualified GHC

import Distribution.Simple.LocalBuildInfo

import Data.IORef

data SessionState 
  = SessionState {
      scionLogLevel :: Int,
      localBuildInfo :: Maybe LocalBuildInfo
    }

mkSessionState :: IO (IORef SessionState)
mkSessionState = newIORef (SessionState 1 Nothing)

newtype ScionM a
  = ScionM { unScionM :: IORef SessionState -> Ghc a }

instance Monad ScionM where
  return x = ScionM $ \_ -> return x
  (ScionM ma) >>= fb = 
      ScionM $ \s -> do
        a <- ma s 
        unScionM (fb a) s             

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
modifySessionState mod = 
    ScionM $ \r -> liftIO $ do s <- readIORef r; writeIORef r $! mod s

getSessionState :: ScionM SessionState
getSessionState = ScionM $ \s -> liftIO $ readIORef s

gets :: (SessionState -> a) -> ScionM a
gets sel = getSessionState >>= return . sel

setSessionState :: SessionState -> ScionM ()
setSessionState s' = ScionM $ \r -> liftIO $ writeIORef r s'
