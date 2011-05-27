{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Main where

import System.Environment (getArgs)
import Text.JSON (JSValue, decodeStrict, encodeStrict, showJSON, readJSON)
import qualified Text.JSON as JSON
import Scion.Types.Note
import Scion.Types.Compiler ( Extension, extensionName )
import Scion.Types.Monad hiding ( catch )
import Scion.Types.Session hiding ( catch )
import Scion.Cabal
import Scion.Session

import Control.Applicative
--import Control.Exception ( throwIO, handle, IOException )
import Data.AttoLisp ( FromLisp(..), ToLisp(..) )
import Data.Bits ( shiftL, (.|.) )
import Data.Monoid
import Data.String
--import Data.Char ( chr )
import Network ( listenOn, PortID(..) )
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Numeric ( showHex )
import System.IO
import qualified Network.Socket.ByteString.Lazy as NL
import qualified Data.AttoLisp as L
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S ( pack )
import qualified Data.MultiSet as MS
import qualified Data.Text as T

import ServerTypes
import ProtocolJSON

data ConnectionMode
  = TcpIp AutoSearchPorts PortNumber
  | StdInOut
  deriving Show

type AutoSearchPorts = Bool

data WireFormat
  = Json
  | Lisp
  -- TODO: Maybe add a Thrift/protobuf based binary format?

type KeepGoing = Bool

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> serve Lisp (TcpIp True 4040)
    ["-json"] -> serve Json (TcpIp True 4040)
    as -> error $ "unexpected args: " ++ show as

serve :: WireFormat -> ConnectionMode -> IO ()
serve wireFormat (TcpIp auto nr) = do
  sock <- if auto then
            listenOnOneOf (map PortNumber [nr .. 0xffff])
          else
            listenOn (PortNumber nr)
  realPort <- socketPort sock
  -- This output is important, it's expected by Emacs.
  hSetBuffering stdout LineBuffering
  putStrLn $ "=== Listening on port: " ++ show realPort
  hFlush stdout -- required because line buffering has been set?
  let loop = do
        handle (\(_e :: IOException) -> do
                   putStrLn "Connection terminated.  Waiting for next client."
                   loop) $ do
          (sock', _addr) <- accept sock
          keep_going <- mainLoop sock' wireFormat
          if keep_going then loop else return ()
  loop
serve _ StdInOut = do
  error "Connection mode input/output not currently supported"

-- | Attempt to listen on each port in the list in turn.
listenOnOneOf :: [PortID] -> IO Socket
listenOnOneOf [] = error "Could not find free port"
listenOnOneOf (p:ps) =
  listenOn p `catch`
    (\(ex :: IOError) ->
      if null ps then throwIO ex else listenOnOneOf ps)

mainLoop :: Socket -> WireFormat -> IO KeepGoing
mainLoop sock Lisp = runScion $ do
   setVerbosity deafening
   loop
 where
   loop = do
     hdr <- io $ recv sock 6
     let mb_len = decodeLen hdr
     case mb_len of
       Nothing -> return True
       Just len -> do
         msg <- io $ recv sock len
         io $ putStr $ "==> [" ++ show len ++ "] "
         io $ B.putStrLn msg
         case parseRequest msg of
           Left err_msg -> do
             io $ putStrLn $ "ParseError: " ++ err_msg
             io $ sendResponse sock invalidRequestId 
                    (Error ("ParseError: " ++ err_msg))
             loop
           Right (Request QuitServer _ _ reqId) -> do
             io $ sendResponse sock reqId (Ok RQuitting)
             return False
           Right (Request cmd _ sessionId reqId) -> do
             -- TODO: Handle exceptions
             mb_resp <- ignoreMostErrors $ handleRequest cmd sessionId
             case mb_resp of
               Right resp -> do
                 io $ sendResponse sock reqId (Ok resp)
                 loop
               Left err_msg -> do
                 io $ sendResponse sock reqId (Error err_msg)
                 loop
mainLoop _sock Json = runScion $ do
    -- json used by Vim backend
    io $ putStrLn $ "starting json loop"
    h <- io $ socketToHandle _sock ReadWriteMode
    io $ hSetBuffering h LineBuffering
    loop h
    return True
  where loop :: Handle -> ScionM ()
        loop h = do
            line <- io $ hGetLine h
            io $ putStrLn $ "debug: got request :" ++ line
            reply :: JSValue <- case decodeStrict line of
              JSON.Error s -> return $ showJSON $ (Left ("JSON decoding error: " ++ s ++ "\n" ++ jsonSamples) :: Either String String)
              JSON.Ok (mbSessionId, cmd) -> do
                server_response <- ignoreMostErrors $ handleRequest cmd mbSessionId
                return $ showJSON $ server_response

            let s = encodeStrict reply
            io $ putStrLn $ "debug: replying :" ++ s
            io $ hPutStrLn h s
            loop h
        jsonSamples = concatMap enc
                                [ (Nothing, ConnectionInfo)
                                , (Just (SessionId 2), ListAvailConfigs (T.pack "file.cabal"))
                                ]
            where enc = ( ++ "\n") . encodeStrict

sendResponse :: Socket -> RequestId -> Response -> IO ()
sendResponse sock reqId resp =
  let !str = encodeResponse reqId resp
  in do
    let len = (fromIntegral $ BL.length str)
    putStr $ "<== [" ++ show len ++ "] "
    BL.putStrLn str
    n <- send sock (encodeLen len)
    m <- NL.send sock str
    putStrLn $ " [Sent: " ++ show n ++ "+" ++ show m ++ "]"
    return ()

encodeLen :: Int -> B.ByteString
encodeLen n =
  let s = showHex n "" in
  S.pack (replicate (6 - length s) '0' ++ s)

-- | Decode a 6 digit hexadecimal number.
decodeLen :: B.ByteString -> Maybe Int
decodeLen b | B.length b /= 6 = Nothing
decodeLen bs = go bs (0 :: Int)
 where
   go b !acc = case B.uncons b of
     Nothing -> Just acc
     Just (w, b')
       | w >= 48 && w <= 57 ->   -- '0'..'9'
         go b' ((acc `shiftL` 4) .|. (fromIntegral w - 48))
       | w >= 97 && w <= 102 ->  -- 'a'..'f'
         go b' ((acc `shiftL` 4) .|. (fromIntegral w - 87))
       | w >= 65 && w <= 70 ->   -- 'A'..'F'
         go b' ((acc `shiftL` 4) .|. (fromIntegral w - 55))
       | otherwise -> Nothing

{-
decodeLen (B.pack (map (fromIntegral . ord) "00000a") == Just 10)
-}

newtype RequestId = RequestId Integer
  deriving (Show)

invalidRequestId :: RequestId
invalidRequestId = RequestId (-1)

data Request
  = Request ServerCommand (Maybe T.Text) (Maybe SessionId) RequestId
  deriving Show


data Response
  = Ok ServerResponse
  | Error String
  | Abort

instance ToLisp RequestId where
  toLisp (RequestId n) = toLisp n

instance FromLisp RequestId where
  parseLisp e = RequestId <$> parseLisp e

instance FromLisp Request where
  parseLisp e = L.struct ":emacs-rex" Request e

instance FromLisp ServerCommand where
  parseLisp e =
    L.struct "connection-info" ConnectionInfo e <|>
    L.struct "list-supported-languages" ListSupportedLanguages e <|>
    L.struct "quit" QuitServer e <|>
    L.struct "list-cabal-components" ListAvailConfigs e <|>
    L.struct "create-session" CreateSession e <|>
    L.struct "file-modified" FileModified e <|>
    (case e of
        L.List (L.Symbol nm:_) ->
          fail $ "Unknown server command: " ++ T.unpack nm
        _ ->
          fail "Invalid command syntax")

instance ToLisp Response where
  toLisp (Ok a)      = L.mkStruct ":ok" [toLisp a]
  toLisp (Error msg) = L.mkStruct ":error" [L.String (T.pack msg)]
  toLisp Abort       = L.mkStruct ":abort" []

instance ToLisp ServerResponse where
  toLisp (RConnectionInfo protoVersion) =
    L.List [L.Symbol ":pid", L.Number 31337,
            L.Symbol ":version", toLisp protoVersion]
  toLisp (RSupportedLanguages exts) = toLisp exts
  toLisp RQuitting = L.nil
  toLisp (RFileConfigs confs) =
    toLisp confs
  toLisp (RSessionCreated sid success notes graph) =
    L.List [toLisp sid, toLisp success, toLisp notes, toLisp graph]
  toLisp (RFileModifiedResult inGraph notes) =
    L.List [toLisp inGraph, toLisp notes]
    
instance ToLisp SessionConfig where
  toLisp (FileConfig file flags) =
    L.List [L.Symbol ":file", fromString file,
            toLisp (map (toLisp . T.pack) flags)]
  toLisp conf@CabalConfig{} =
    case sc_component conf of
      Library -> L.List [L.Symbol ":library",
                         toLisp (T.pack (sc_cabalFile conf))]
      Executable e ->
        L.List [L.Symbol ":executable", fromString e,
                toLisp (T.pack (sc_cabalFile conf))]
  toLisp EmptyConfig{} = error "Cannot serialise EmptyConfig"

instance FromLisp SessionConfig where
  parseLisp e =
    L.struct ":library" mkLibrary e <|>
    L.struct ":executable" mkExecutable e <|>
    L.struct ":file" (\f -> FileConfig (T.unpack f) []) e
   where
     mkLibrary :: T.Text -> SessionConfig
     mkLibrary cabalFile = componentToSessionConfig (T.unpack cabalFile) Library
     
     mkExecutable :: T.Text -> T.Text -> SessionConfig
     mkExecutable exeName cabalFile =
       componentToSessionConfig (T.unpack cabalFile)
                                (Executable (T.unpack exeName))

instance ToLisp SessionId where
  toLisp = toLisp . unsafeSessionIdToInt

instance FromLisp SessionId where
  parseLisp e = unsafeSessionIdFromInt <$> parseLisp e

instance ToLisp Extension where
  toLisp = toLisp . extensionName

instance ToLisp a => ToLisp (MS.MultiSet a) where
  toLisp = toLisp . MS.toList

instance ToLisp Note where
  toLisp (Note knd loc msg) =
    L.mkStruct "note" [toLisp knd, toLisp loc, toLisp msg]

instance ToLisp NoteKind where
  toLisp ErrorNote = L.Symbol ":error"
  toLisp WarningNote = L.Symbol ":warning"
  toLisp InfoNote = L.Symbol ":info"
  toLisp OtherNote = L.Symbol ":other"

instance ToLisp Location where
  toLisp loc | not (isValidLoc loc) =
    L.mkStruct ":no-loc" [toLisp (T.pack (noLocText loc))]
  toLisp loc | (src, sl, sc, el, ec) <- viewLoc loc =
    L.mkStruct ":loc" (toLisp src : map toLisp [sl, sc, el, ec])

instance ToLisp LocSource where
  toLisp (FileSrc path) =
    L.mkStruct ":file" [toLisp (T.pack (toFilePath path))]
  toLisp (OtherSrc txt) =
    L.mkStruct ":other" [toLisp (T.pack txt)]

instance ToLisp ModuleSummary where
  toLisp modsum =
    L.mkStruct "modsum" [toLisp (ms_module modsum),
                         toLisp (T.pack $ ms_location modsum)]

instance ToLisp ModuleName where
  toLisp modname = toLisp (moduleNametoText modname)

--instance From

parseRequest :: B.ByteString -> Either String Request
parseRequest chunk =
  case A.parseOnly L.lisp chunk of
    Left msg -> Left msg
    Right lsp ->
      case L.fromLisp lsp of
        L.Success req -> Right req
        L.Error msg -> Left msg

encodeResponse :: RequestId -> Response -> BL.ByteString
encodeResponse reqId resp =
  L.encode (L.List [return_kw, toLisp resp, toLisp reqId])
 where
   return_kw = L.Symbol ":return"
 {- 
test1 =
  case A.parseOnly L.lisp (S.pack "(list-supported-languages)") of
    Left msg -> putStrLn msg
    Right lsp ->
      case L.fromLisp lsp :: L.Result ServerCommand of
        L.Success c -> print c
        L.Error msg -> putStrLn msg
-}
-----------------------------------------------------------------------

scionProtocolVersion :: Int
scionProtocolVersion = 2

handleRequest :: ServerCommand -> Maybe SessionId -> ScionM ServerResponse
handleRequest ConnectionInfo _ = do
  return (RConnectionInfo scionProtocolVersion)
handleRequest ListSupportedLanguages _ =
  RSupportedLanguages <$> supportedLanguagesAndExtensions
handleRequest (ListAvailConfigs file) _ =
  RFileConfigs <$> cabalSessionConfigs (T.unpack file)
handleRequest (CreateSession conf) _ = do
  sid <- createSession conf
  notes <- sessionNotes sid
  mods <- sessionModules sid
  return (RSessionCreated sid (not (hasErrors notes)) notes mods)
handleRequest (FileModified file) (Just sid) = do
  fileModified sid (T.unpack file)
  let fileInModuleGraph = True -- FIXME: find out
  RFileModifiedResult fileInModuleGraph <$> sessionNotes sid
handleRequest (FileModified file0) Nothing = do
  let file = T.unpack file0
  ss <- fileSessions file
  case ss of
    [] ->
      return $ RFileModifiedResult False mempty
    sid:_ -> do
      fileModified sid file
      RFileModifiedResult True <$> sessionNotes sid

handleRequest QuitServer _ =
  error "handleRequest: should not have reached this point"
