{-# LANGUAGE BangPatterns, DeriveDataTypeable, ScopedTypeVariables,
             TypeFamilies, PatternGuards, CPP #-}
-- |
-- Module      : Scion.Server.Emacs
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : pre-alpha
-- Portability : portable
--
-- An example server which will talk to different backends
-- The first handshake is done this way:
-- The client sends : "select scion-server protocol: name version"
-- where name and version specify the protocol to be used.
-- the server replies in any case by either
-- "ok\n" or "failure : message\n"
-- From then on the specific protocol handler takes over control
--
-- multiple connections to the same server are not yet supported
-- because I don't know yet in detail when ghc api calls can be made
-- concurrently.. Maybe using an MVar is an option (TODO)

module Main where
import Prelude hiding ( log )
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess)
import System.IO (stdin, stdout)
import qualified System.Log.Logger as HL
import qualified System.Log.Handler.Syslog as HL
import qualified Data.ByteString.Char8 as S
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Data.List (isPrefixOf, break)
import Data.Foldable (foldrM)
import qualified Control.Exception as E


import Control.Monad ( when, forever )

import System.Console.GetOpt

import MonadUtils ( liftIO )
import qualified Scion.Server.ProtocolEmacs as Emacs
import qualified Scion.Server.ProtocolVim as Vim
import qualified Scion.Server.ConnectionIO as CIO
import Scion (runScion)

log = HL.logM __FILE__
logInfo = log HL.INFO
logError = log HL.ERROR

-- how should the client connect to the server? 
-- if you're paranoid about your code Socketfile or StdInOut
-- will be the most secure choice.. (Everyone can connect via TCP/IP at the
-- moment)
data ConnectionMode = TCPIP PortNumber
                  | StdInOut
#ifndef mingw32_HOST_OS
                  | Socketfile FilePath
#endif
  deriving Show

data StartupConfig = StartupConfig {
     connectionMode :: ConnectionMode,
     showHelp :: Bool
  } deriving Show
defaultStartupConfig = StartupConfig ( TCPIP (fromInteger 4005)) False

-- options :: [OptDescr (Options -> Options)]
options =
     [ Option ['p']     ["port"]
       (ReqArg (\o opts -> return $ opts { connectionMode = (TCPIP . fromInteger) (read o) }) "8010")
       "listen on this TCP port"
     , Option ['i'] ["stdinout"]
       (NoArg (\opts -> return $ opts { connectionMode = StdInOut}))
       "client must connect to stdin and stdout (untested)"
#ifndef mingw32_HOST_OS
     , Option ['s'] ["socketfile"]
       (ReqArg (\o opts -> return $ opts { connectionMode = Socketfile o}) "/tmp/scion-io")
       "listen on this socketfile (untested)"
#endif
     , Option ['h'] ["help"] (NoArg (\opts -> return $ opts { showHelp = True } )) "show this help"
     ]

initializeLogging = do
  stdout <- HL.openlog "" [] HL.USER HL.DEBUG
  HL.updateGlobalLogger "" (HL.addHandler stdout)   -- add a default logger
  HL.updateGlobalLogger "" (HL.setLevel HL.DEBUG)

helpText = do
    pN <- getProgName
    let header = unlines [ "usage of scion server (executable :"  ++ pN  ++ ")" ]
    return $ usageInfo header options

serve :: ConnectionMode -> IO ()
serve (TCPIP nr) = do
  sock <- liftIO $ listenOn (PortNumber 4005)
  (sock', _addr) <- liftIO $ accept sock
  handleClient sock'
serve StdInOut = handleClient (stdin, stdout)
#ifndef mingw32_HOST_OS
serve (Socketfile file) = do
  sock <- liftIO $ listenOn (PortNumber 4005)
  forever $ do
    -- no multithreading for now (I don't know yet when it may be used.. the
    -- ghc library is using some IO refs) 
    (sock', _addr) <- liftIO $ accept sock
    handleClient sock'
#endif


-- does the handshaking and then runs the protocol implementation 
handleClient :: (CIO.ConnectionIO con) => con -> IO ()
handleClient con = do
  greeting <- CIO.getLine con
  let prefix = S.pack "select scion-server protocol:" 
      quit :: String -> IO ()
      quit msg = do
        CIO.putLine con (S.pack msg)
        logError msg
      handle "vim" version = runScion $ Vim.handle con version
      handle "emacs" version = runScion $ Emacs.handle con version
      handle name _ = quit $ "unkown protocol type : " ++ name
        
  if S.isPrefixOf prefix greeting 
    then let (a,b) =  S.break (== ' ') (S.drop (S.length prefix) greeting)
         in handle (S.unpack a) (S.unpack b)
    else quit $ "prefix " ++ (show $ (S.unpack prefix)) ++ " expected, but got : " ++ (S.unpack greeting)

main = do

  -- logging 
  initializeLogging

  -- cmd opts 
  (opts, nonOpts, err_msgs) <- fmap (getOpt Permute options) getArgs

  when ((not . null) nonOpts) $ logError $ "no additional arguments expected, got: " ++ (show nonOpts)

  startupConfig <- foldrM ($) defaultStartupConfig opts

  -- help 
  when (showHelp startupConfig) $ helpText >>= putStrLn >> exitSuccess

  -- start server 
  logInfo "starting server"
  -- E.handle (\(e :: SomeException) ->  "shutting down server due to exception "  ++ show e) $
  do
      log HL.DEBUG $ "opts: " ++ (show startupConfig)
      serve (connectionMode startupConfig)



