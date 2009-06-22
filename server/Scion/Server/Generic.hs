module Scion.Server.Generic 
  ( handle
  ) where

import Prelude hiding ( log )

import Scion
import Scion.Server.ConnectionIO as CIO
import Scion.Server.Commands

import Text.JSON
import qualified Data.ByteString.Lazy.Char8 as S
import qualified Data.ByteString.Lazy.UTF8 as S
import qualified System.Log.Logger as HL

log :: HL.Priority -> String -> IO ()
log = HL.logM "protocol.generic"
logDebug :: MonadIO m => String -> m ()
logDebug = liftIO . log HL.DEBUG


handle :: (ConnectionIO con) =>
          con
       -> Int
       -> ScionM ()
handle con 0 = do
   loop
  where
   loop = do
     -- TODO: don't require line-based input
     str <- liftIO $ CIO.getLine con
     logDebug $ "parsing command: " ++ show str
     let mb_req = decodeStrict (S.toString str)
     (resp, keep_going) 
         <- case mb_req of
              Error _ -> return (malformedRequest, True)
              Ok req -> handleRequest req
     let resp_str = encodeStrict resp
     logDebug $ show resp_str
     liftIO $ CIO.putLine con (S.fromString resp_str)
     --logDebug $ "sent response"
     if keep_going then loop else do 
       --logDebug "finished serving connection."
       return ()

handle con unknownVersion =
  -- handshake failure, don't accept this client version 
  liftIO $ CIO.putLine con $ 
    S.pack $ "failure: Don't know how to talk to client version "
      ++ (show unknownVersion)
