{-# LANGUAGE CPP, DeriveDataTypeable #-}
-- | Command line options for the @scion-server@ binary.
module Scion.Server.Options 
  ( ProxyMode(..), WireFormat(..),
    parseArgs
  )
where

import Data.Data
import Network.Socket ( PortNumber(..) )
import System.Console.CmdArgs
import System.IO ( stderr, hPutStrLn )
import System.Exit ( exitFailure )

data ProxyMode
  = StdIO { wire_format :: WireFormat
          , worker_path :: FilePath }
  | Socket { port :: Int
           , scan_port :: Bool
           , wire_format :: WireFormat
           , worker_path :: FilePath }
#ifndef mingw32_HOST_OS
  | SocketFile { path :: FilePath 
               , wire_format :: WireFormat
               , worker_path :: FilePath }
#endif
  deriving (Eq, Show, Data, Typeable)

data WireFormat
  = Json
  | Lisp
  | Vim
  deriving (Eq, Show, Data, Typeable)

defaultPort :: Int
defaultPort = 4040  -- unassigned according to Wikipedia

stdioMode =
  mode $ StdIO
    { wire_format = enum Json
        [ Json &= text "Use JSON as wire format (default)"
        , Lisp &= text "Use Lisp S-expressions as wire format"
        , Vim &= text "Use a wire format readable by Vimscript"
        ]
    , worker_path = def &= typFile & empty "scion-worker"
                      & text "Path to the scion-worker executable."
    } &= prog "scion-server " & text "Communicate with server via stdio"
       & defMode

socketMode =
  mode $ Socket
    { port = defaultPort &= typ "PORT" & text "Use this port"
    , scan_port = False &= text "Automatically scan for a free port"
    , wire_format = enum Json
        [ Json &= text "Use JSON as wire format (default)"
        , Lisp &= text "Use Lisp S-expressions as wire format"
        , Vim &= text "Use a wire format readable by Vimscript"
        ]
    , worker_path = def &= typFile & empty "scion-worker" 
                      & text "Path to the scion-worker executable."
    } &= text "Communicate with server via TCP/IP."

modes = [stdioMode, socketMode]

verifyMode :: ProxyMode -> Maybe String
verifyMode Socket{ port = p }
  | p >= 1 && p < 65535 = Nothing
  | otherwise = Just "Invalid port number.  Must be within 1..65535."
verifyMode _ = Nothing

parseArgs :: IO ProxyMode
parseArgs = do
  mode <- cmdArgs "Scion Server v0.2" modes
  case verifyMode mode of
    Just msg -> do
      hPutStrLn stderr msg
      exitFailure
    _ -> return mode
    
