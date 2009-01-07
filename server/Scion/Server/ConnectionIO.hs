{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module      : Scion.Server.ConnectionIO
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : experimental
-- Portability : portable
--
-- abstraction over Socket and Handle IO


module Scion.Server.ConnectionIO (
  ConnectionIO(..)
)where
import Control.Exception (throw, IOException, Exception)
-- import System.IO.Error (mkIOError, IOErrorType(..) )
import Prelude hiding (log)
import System.IO (Handle, hClose, hPutStr, hPutStrLn)
import Control.Monad (when)
import Network.Socket (Socket, sClose)
import Network.Socket.ByteString (recv, send)
import qualified System.Log.Logger as HL
import qualified Data.ByteString.Char8 as S

log = HL.logM "__FILE__"
logError = log HL.ERROR
logWarning = log HL.WARNING

class ConnectionIO con where
  getLine :: con -> IO S.ByteString
  getN :: con -> Int -> IO S.ByteString
  put :: con -> S.ByteString -> IO ()
  putLine :: con -> S.ByteString -> IO ()
  putLine c s = put c s >> put c (S.singleton '\n')

-- (stdin,stdout) implemenation 
instance ConnectionIO (Handle, Handle) where
  getLine (i, _) = S.hGetLine i
  getN (i,_) = S.hGet i
  put (_,o) = S.hPutStr o
  putLine (_,o) = S.hPutStrLn o

-- Socket.ByteString implemenation 
instance ConnectionIO Socket where
  getLine con =
    -- not optimized. Does this matter, do we receive huge data chunks? (TODO)
    let nl = (S.pack "\n")
        gl got = do
          c <- getN con 1
          if c == nl then return got
                     else return $ S.concat [got, c] -- bad performance, memcpy !
    in do b <- gl S.empty
          when (S.length b > 1024) $ 
            logWarning "received chunk bigger than 1k. Check performance of implementation"
          return b
  getN con len = recv con len
  put con str = do
    let l = S.length str
    sent <- send con str
    when (sent /= l) $ do
      logError $ (show l) ++ " bytes to be sent but could only sent : " ++ (show sent)
      -- is there a better excption which should be thrown instead?  (TODO)
      -- throw $ mkIOError ResourceBusy ("put in " ++ __FILE__) Nothing Nothing
