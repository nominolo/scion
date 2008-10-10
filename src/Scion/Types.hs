module Scion.Types where

import GHC ( Ghc )
import qualified GHC

data SessionState 
  = SessionState

newtype ScionM a
  = ScionM { 
      unScionM :: forall r.
                  SessionState
               -> (SessionState -> a -> Ghc r)
               -> Ghc r
    }

instance Monad ScionM where
  return x = ScionM $ \s ka -> ka s x
  (ScionM ma) >>= fb = 
      ScionM $ \s kb ->
          ma s $ \s' a ->
          unScionM (fb a) s' kb             

instance Functor ScionM where
  fmap f (ScionM ma) =
      ScionM $ \s kb ->
          ma s $ \s' a -> kb s' (f a)

modifySessionState :: (SessionState -> SessionState) -> ScionM ()
modifySessionState mod = 
    ScionM $ \s k -> 
        let s' = mod s in s' `seq` k s' ()

getSessionState :: ScionM SessionState
getSessionState = ScionM $ \s k -> k s s

gets :: (SessionState -> a) -> ScionM a
gets sel = getSessionState >>= return . sel

setSessionState :: SessionState -> ScionM ()
setSessionState s' = ScionM $ \s k -> k s' ()
