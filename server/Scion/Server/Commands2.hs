{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Scion.Server.Commands2 where

import GHC (GhcException(..))
import Exception (gcatch, ghandle)
import Control.Exception ( throwIO, SomeException )
import System.Exit (ExitCode)
import Scion.Types
import Scion.Server.Message
import Data.String ( fromString )

import DynFlags ( supportedLanguages, allFlags )

import qualified Data.Map             as M
import qualified Data.Text            as T

------------------------------------------------------------------------
type KeepGoing = Bool

handleRequest :: MsgData -> ScionM (MsgData, KeepGoing)
handleRequest (MsgMap m) =
  let request = do
        MsgText method <- M.lookup "method" m
        params <- M.lookup "params" m
        seq_id <- M.lookup "id" m
        return (method, params, seq_id)
  in do
   --liftIO $ print ("got request", request)
   case request of
     Nothing ->
       return (respMalformedRequest, True)
     Just (method, params, seq_id) 
       | method == "quit" ->
         return (mkMap [("version", protocolVersion)
                       ,("result", MsgNull)
                       ,("id", seq_id)],
                 False)
       | otherwise ->
         ghandle (\(e :: SomeException) -> do
                    liftIO $ print e
                    return (toMsg (tshow e), False)) $
           case M.lookup method allCmds of
             Nothing -> do
               liftIO $ putStrLn "Unknown method"
               return (respUnknownMethod seq_id, True)
             Just (Cmd _ arg_parser) ->
               decode_params params arg_parser seq_id
 where
   decode_params MsgNull arg_parser seq_id =
     decode_params (mkMap []) arg_parser seq_id
   decode_params (MsgMap args) arg_parser seq_id =
     case unPa arg_parser args of
       Left err -> do
         --liftIO $ putStrLn "Could not parse args"
         return (respParseParamError seq_id (T.pack err), True)
       Right act -> do
         r <- handleScionException act
         case r of
           Left msg ->
             return (respCommandExecError seq_id msg, True)
           Right a ->
             return 
               (mkMap [("version", protocolVersion)
                      ,("id", seq_id)
                      ,("result", toMsg a)]
               ,True)
   decode_params _ _ seq_id =
     return (respParseParamError seq_id
               "Parameters must be a named map.", True)
                  
--          if method == "ping" then
--            return (mkMap [("version", protocolVersion)
--                          ,("id", seq_id)
--                          ,("result", MsgNull)], True)
--           else
--            return (MsgNull, True)
       
-- | This response is sent if the input request does not follow the
-- standard format.
respMalformedRequest :: MsgData
respMalformedRequest =
  mkMap [("version", protocolVersion)
        ,("error", mkMap $
                   [("name", MsgText "MalformedRequest")
                   ,("message", MsgText "Malformed request.")])]
  
respUnknownMethod :: MsgData -> MsgData
respUnknownMethod seq_id =
  mkMap [("version", protocolVersion)
        ,("id", seq_id)
        ,("error",
          mkMap $
          [("name", "UnknownCommand")
          ,("message", "The requested method is not supported.")])
        ]

respParseParamError :: MsgData -> T.Text -> MsgData
respParseParamError seq_id msg =
  mkMap [("version", protocolVersion)
        ,("id", seq_id)
        ,("error",
          mkMap $
          [("name", "ParseParametersError")
          ,("message", toMsg msg)])]

respCommandExecError :: MsgData -> T.Text -> MsgData
respCommandExecError seq_id msg =
  mkMap [("version", protocolVersion)
        ,("id", seq_id)
        ,("error",
          mkMap $
          [("name", "CommandFailed")
          ,("message", toMsg msg)])]

protocolVersion :: MsgData
protocolVersion = MsgInt 2

tshow :: Show a => a -> T.Text
tshow = T.pack . show

handleScionException :: ScionM a -> ScionM (Either T.Text a)
handleScionException m = ((((do
   r <- m
   return (Right r)
  `gcatch` \(e :: SomeScionException) -> return (Left (tshow e)))
  `gcatch` \(e' :: GhcException) -> 
               case e' of
                Panic _ -> liftIO $ throwIO e'
                InstallationError _ -> liftIO $ throwIO e'
                Interrupted -> liftIO $ throwIO e'
                _ -> return (Left (tshow e')))
  `gcatch` \(e :: ExitCode) -> 
                -- client code may not exit the worker
                return (Left (tshow e)))
  `gcatch` \(e :: IOError) ->
                return (Left (tshow e)))

--   `gcatch` \(e :: SomeException) ->
--                 liftIO (print e) >> liftIO (throwIO e)

----------------------------------------------------------------------
-- * Command Parsing

newtype Pa a = Pa { unPa :: M.Map T.Text MsgData -> Either String a }

instance Monad Pa where
  return x = Pa $ \_ -> Right x
  m >>= k = Pa $ \req -> 
            case unPa m req of
              Left err -> Left err
              Right a -> unPa (k a) req
  fail msg = Pa $ \_ -> Left msg

noArgs :: r -> Pa r
noArgs = return

data Cmd = forall a. Message a => Cmd T.Text (Pa (ScionM a))

cmdName :: Cmd -> T.Text
cmdName (Cmd n _) = n

-- -------------------------------------------------------------------

allCmds :: M.Map T.Text Cmd
allCmds = M.fromList [ (cmdName c, c) | c <- allCommands ]

-- | All Commands supported by this Server.
allCommands :: [Cmd]
allCommands =
  [ cmdPing
  , cmdListSupportedPragmas
  , cmdConnectionInfo
  ]

-- | Used to test whether the server is alive.
cmdPing :: Cmd
cmdPing =
  Cmd "ping" $ noArgs $ return ()

-- | Used by the client to initialise the connection.
--
-- TODO: We might want to intercept this at the proxy side and return
-- a session ID.
cmdConnectionInfo :: Cmd
cmdConnectionInfo = Cmd "connection-info" $ noArgs worker
  where
    worker = let pid = 0 in -- TODO for linux: System.Posix.Internals (c_getpid)
             return $ mkMap
               [("version", MsgList $ map MsgInt [0,2,0,1])
               ,("pid",     pid)]

cmdListSupportedLanguages :: Cmd
cmdListSupportedLanguages = Cmd "list-supported-languages" $ noArgs cmd
  where cmd = return (map T.pack supportedLanguages)

cmdListSupportedPragmas :: Cmd
cmdListSupportedPragmas = 
  Cmd "list-supported-pragmas" $ noArgs $ return supportedPragmas

supportedPragmas :: [T.Text]
supportedPragmas =
    [ "OPTIONS_GHC", "LANGUAGE", "INCLUDE", "WARNING", "DEPRECATED"
    , "INLINE", "NOINLINE", "RULES", "SPECIALIZE", "UNPACK", "SOURCE"
    , "SCC"
    , "LINE" -- XXX: only used by code generators, still include?
    ]
