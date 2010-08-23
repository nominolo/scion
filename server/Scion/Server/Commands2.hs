{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables, PatternGuards #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
-- Warn unused binds is important, because it allows us to notice when
-- we forgot to add a command to 'allCommands'.
module Scion.Server.Commands2 
  ( handleRequest, KeepGoing )
where

import Scion.Server.Message

import Scion.Types
import Scion.Types.Notes
import Scion.Backend
import Scion.Cabal
import Scion.Session

import GHC (GhcException(..))
import Exception (gcatch, ghandle)

import qualified Data.Map             as M
import qualified Data.Text            as T
import qualified Data.MultiSet as MS
import Control.Applicative
import Control.Exception ( throwIO, SomeException )
import Data.String ( fromString )
import Data.Time.Clock  ( NominalDiffTime )
import Data.Typeable ( cast )
import System.Exit (ExitCode)

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

withReq :: (M.Map T.Text MsgData -> Pa a) -> Pa a
withReq f = Pa $ \req -> unPa (f req) req

noArgs :: r -> Pa r
noArgs = return

getArg :: Message a => T.Text -> Pa r -> (a -> b) -> (b -> r) -> Pa r
getArg arg_name not_present trans f = withReq $ \req ->
  case M.lookup arg_name req of
    Nothing -> not_present
    Just x ->
      case fromMsg x of
        Error m ->
          fail $ "could not decode: " ++ show arg_name ++ " - " ++ m
        Ok a -> return (f (trans a))

-- | Combine two arguments.
--
-- TODO: explain type
(<&>) :: (a -> Pa b)
      -> (b -> Pa c)
      -> a -> Pa c
a1 <&> a2 = \f -> do f' <- a1 f; a2 f'

reqArg :: Message a => T.Text -> (a -> r) -> Pa r
reqArg arg_name f = reqArg' arg_name id f

reqArg' :: Message a => T.Text -> (a -> b) -> (b -> r) -> Pa r
reqArg' arg_name trans f =
  getArg arg_name (fail $ "required arg missing: " ++ show arg_name) trans f

optArg :: Message a => T.Text -> a -> (a -> r) -> Pa r
optArg name dflt f = optArg' name dflt id f

optArg' :: Message a => T.Text -> b -> (a -> b) -> (b -> r) -> Pa r
optArg' name dflt trans f =
  getArg name (return (f dflt)) trans f

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
  , cmdConnectionInfo
  , cmdListSupportedLanguages
  , cmdListSupportedPragmas
  , cmdListSupportedFlags
  , cmdListExposedModules
  , cmdLoad
  , cmdListCabalComponents
  , cmdCurrentComponent
  , cmdForceUnload
  , cmdBackgroundTypecheckFile
  , cmdBackgroundTypecheckArbitrary
  , cmdSetVerbosity
  , cmdAddCmdLineFlag
  ]

instance Message ModuleName where
  toMsg mn = MsgText (moduleNameText mn)
  fromMsg (MsgText txt) = pure (mkModuleName txt)
  fromMsg _ = fail $ "ModuleName"

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
               [("version", MsgInt 2)
               ,("pid",     pid)]

cmdListSupportedLanguages :: Cmd
cmdListSupportedLanguages =
  Cmd "list-supported-languages" $ noArgs $ return supportedLanguages

cmdListSupportedPragmas :: Cmd
cmdListSupportedPragmas = 
  Cmd "list-supported-pragmas" $ noArgs $ return supportedPragmas

cmdListSupportedFlags :: Cmd
cmdListSupportedFlags =
  Cmd "list-supported-flags" $ noArgs $ return supportedOptions

cmdListExposedModules :: Cmd
cmdListExposedModules =
  Cmd "list-exposed-modules" $ noArgs $ allExposedModuleNames

instance Message Component where
  toMsg (Component comp)
    | Just (c :: CabalComponent) <- cast comp = toMsg c
    | Just (c :: FileComp) <- cast comp = toMsg c
  toMsg _ = error "Cannot encode component."
  fromMsg m
    | Ok (c :: CabalComponent) <- fromMsg m
    = return $ Component c
    | Ok (c :: FileComp) <- fromMsg m
    = return $ Component c
    | otherwise
    = fail "Component"

instance Message FileComp where
  toMsg (FileComp f) = mkMap [("file", fromString f)]
  fromMsg m
    | Ok f <- decodeKey m "file" = return (FileComp (T.unpack f))
  fromMsg _ = fail "FileComp"

instance Message CabalComponent where
  toMsg (Library f) =
    mkMap [("library", MsgNull), ("cabal-file", fromString f)]
  toMsg (Executable f n) =
    mkMap [("executable", fromString n), ("cabal-file", fromString f)]
  fromMsg m
    | Ok () <- decodeKey m "library", Ok f <- decodeKey m "cabal-file"
    = return $ Library (T.unpack f)
    | Ok e <- decodeKey m "executable", Ok f <- decodeKey m "cabal-file"
    = return $ Executable (T.unpack f) (T.unpack e)
  fromMsg _ = fail "CabalComponent"

instance Message CompilationResult where
  toMsg (CompilationResult suc notes time) =
    mkMap [("succeeded", toMsg suc)
          ,("notes", toMsg notes)
          ,("duration", toMsg time)]
  fromMsg m =
    CompilationResult <$> decodeKey m "succeeded"
                      <*> decodeKey m "notes"
                      <*> decodeKey m "duration"

instance (Ord a, Message a) => Message (MS.MultiSet a) where
  toMsg ms = toMsg (MS.toList ms)
  fromMsg m = MS.fromList <$> fromMsg m

instance Message Note where
  toMsg (Note note_kind loc msg) =
    mkMap [("kind", toMsg note_kind)
          ,("location", toMsg loc)
          ,("message", fromString msg)]
  fromMsg m =
    Note <$> decodeKey m "kind"
         <*> decodeKey m "location"
         <*> (T.unpack <$> decodeKey m "message")

instance Message NoteKind where
  toMsg ErrorNote = "error"
  toMsg WarningNote = "warning"
  toMsg InfoNote = "info"
  toMsg OtherNote = "other"
  fromMsg (MsgText nk) =
    let mp = M.fromList [("error", ErrorNote)
                        ,("warning", WarningNote)
                        ,("info", InfoNote)
                        ,("other", OtherNote)]
    in case M.lookup nk mp of
         Nothing -> fail "NoteKind"
         Just k -> return k

instance Message Location where
  toMsg loc | not (isValidLoc loc) =
    mkMap [("no-location", fromString (noLocText loc))]
  toMsg loc | (src, l0, c0, l1, c1) <- viewLoc loc =
    mkMap [case src of
             FileSrc f -> ("file", fromString (toFilePath f))
             OtherSrc s -> ("other", fromString s)
          ,("region", toMsg [l0,c0,l1,c1])]
  fromMsg m
    | Ok fp <- decodeKey m "file"
    , Ok [l0,c0,l1,c1] <- decodeKey m "region"
    = return $ mkLocation (FileSrc (mkAbsFilePath "/" (T.unpack fp))) -- XXX: Why the "/"?
                          l0 c0 l1 c1
    | Ok s <- decodeKey m "other"
    , Ok [l0,c0,l1,c1] <- decodeKey m "region"
    = return $ mkLocation (OtherSrc (T.unpack s)) l0 c0 l1 c1
    | Ok s <- decodeKey m "no-location"
    = return $ mkNoLoc (T.unpack s)
  fromMsg _ = fail "Location"

instance Message NominalDiffTime where
  toMsg ndt = MsgDouble $ fromRational $ toRational ndt
  fromMsg (MsgInt n) = return $ fromIntegral n
  fromMsg (MsgDouble n) = return $ fromRational $ toRational n
  fromMsg _ = fail "NominalDiffTime"

cmdListCabalComponents :: Cmd
cmdListCabalComponents =
    Cmd "list-cabal-components" $ reqArg "cabal-file" $ cmd
  where cmd cabal_file = cabalProjectComponents (T.unpack cabal_file)

cmdLoad :: Cmd
cmdLoad = 
  Cmd "load" $ reqArg "component" <&>
               optArg "output" False $
      loadComponent'

cmdCurrentComponent :: Cmd
cmdCurrentComponent = Cmd "current-component" $ noArgs $ getActiveComponent

cmdForceUnload :: Cmd
cmdForceUnload = Cmd "force-unload" $ noArgs $ unload
cmdBackgroundTypecheckFile :: Cmd
cmdBackgroundTypecheckFile =
    Cmd "background-typecheck-file" $ reqArg "file" $ cmd
  where cmd fname = do
          either (Left . T.pack) Right <$>
            backgroundTypecheckFile (T.unpack fname)

-- | TODO: Horribly inefficient -- we're going from Text to String to
-- GHC's StringBuffer.  What we actually want is a diff.
cmdBackgroundTypecheckArbitrary :: Cmd
cmdBackgroundTypecheckArbitrary =
  Cmd "background-typecheck-arbitrary" $
      reqArg "file" <&>
      reqArg "contents" $ cmd
 where
   cmd fname contents =
     either (Left . T.pack) Right <$>
       backgroundTypecheckArbitrary (T.unpack fname) (T.unpack contents)

cmdSetVerbosity :: Cmd
cmdSetVerbosity = 
    Cmd "set-verbosity" $
        reqArg "level" <&>
        optArg' "backend-level" Nothing Just
        $ cmd
  where 
    cmd :: Int -> Maybe Int -> ScionM ()
    cmd v mb_v' = do
      setVerbosity (intToVerbosity v)
      case mb_v' of
        Just v' -> setGHCVerbosity v'
        _ -> return ()

cmdAddCmdLineFlag :: Cmd
cmdAddCmdLineFlag = 
    Cmd "add-command-line-flag" $
      optArg "flag" "" <&>
      optArg "flags" [] $ cmd
  where 
    cmd flag flags = do
      addCmdLineFlags $ map T.unpack $ if flag == "" then flags else flag:flags
      return ()
