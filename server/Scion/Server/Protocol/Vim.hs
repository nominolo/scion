{-# LANGUAGE ScopedTypeVariables, CPP #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances,
    FlexibleInstances, OverlappingInstances #-}
-- |
-- Module      : Scion.Server.ProtocolVim
-- License     : BSD-style
--
-- Maintainer  : marco-oweber@gmx.de
-- Stability   : experimental
-- Portability : portable
--
-- talk to vim
-- each request or response is a vimtype (:h string and :h eval). Messages are
-- separated by a newline character "\n"

module Scion.Server.Protocol.Vim where
import Scion.Server.Protocol (scionVersion)
import qualified Scion.Server.ConnectionIO as CIO
import Scion.Server.Commands (supportedPragmas, allExposedModules)
import Scion.Server.ConnectionIO (ConnectionIO(..))
import Scion.Types (ScionM, Component(..), gets, bgTcCache, BgTcCache(..), CompilationResult(..))
import Scion.Inspect ( prettyResult )
import Scion.Inspect.Find ( overlaps, findHsThing, pathToDeepest)
import Scion.Inspect.TypeOf ( typeOf )
import Scion.Configure (configureCabalProject)
import Scion.Utils ( unqualifiedForModule )
import Scion.Session (preprocessPackage, currentCabalPackage, loadComponent,
                      backgroundTypecheckFile, unload, setGHCVerbosity, addCmdLineFlags)
import FastString (fsLit, unpackFS)

import Control.Monad (forever, liftM)
import Control.Exception.Base (Exception)
--import Control.Monad.Trans (lift)
import qualified Control.Exception as E 
import Prelude hiding (log)
import qualified System.Log.Logger as HL

import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M
import Data.List (intercalate, nub)
import Data.Time.Clock  ( NominalDiffTime )

import DynFlags ( supportedLanguages, allFlags )
import InteractiveEval ( getNamesInScope )
import qualified Outputable as O
import GHC
import Exception (ghandle)
import PprTyThing (pprTypeForUser)
import ErrUtils (WarningMessages, ErrorMessages, ErrMsg(..) )
import Bag (bagToList, Bag)

import GHC.Read (readPrec)
import MonadUtils
import SrcLoc (SrcSpan(..))
import Text.ParserCombinators.ReadPrec (readPrec_to_S, minPrec)
import Distribution.Text ( display )
import qualified Distribution.PackageDescription as PD

import GHC.SYB.Utils (showData)

-- think about using another parser so that this dependency can be removed?
import Text.ParserCombinators.Parsec.Error (ParseError)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char

import Text.ParserCombinators.ReadP (skipSpaces)

log = HL.logM __FILE__
logInfo = log HL.INFO
logError = log HL.ERROR
logDebug = log HL.DEBUG

data VimCommand = VimCommand String (M.Map VimType VimType -> ScionM VimType)

-- to be synchronized with allCommands 
vimCommands :: [VimCommand]
vimCommands =
    [ cmdConnectionInfo
    , cmdOpenCabalProject
    , cmdLoadComponent
    , cmdListSupportedLanguages
    , cmdListSupportedPragmas
    , cmdListSupportedFlags
    , cmdListRdrNamesInScope
    , cmdListExposedModules
    , cmdSetGHCVerbosity
    , cmdBackgroundTypecheckFile
    , cmdForceUnload
    , cmdAddCmdLineFlag
    , cmdThingAtPoint
    -- , cmdDumpSources
    ]

------------------------------------------------------------------------------

-- | get request, parse it and send reply
handle :: (ConnectionIO con) => con -> String -> ScionM ()
handle con "0" = do
    -- handshake ok, accept this client version 
    liftIO $ CIO.putLine con $ S.pack "ok"

    -- handle requests:
    forever $ do
      -- read request line 
      l <- liftM S.unpack $ liftIO $ CIO.getLine con
      liftIO $ logDebug $ "got request str" ++ l

      -- handle it
      vimTypeReply <- handleFailure $ case parseVim l of
        Right (VDict map') -> case M.lookup (VString "request") map' of
          Nothing -> fail "key request missing "
          Just r -> do
          rs <- fromString r
          case lookup rs (map (\(VimCommand r a) -> (r, a)) vimCommands) of
            Just a -> a map'
            Nothing -> fail $ "unkown request: " ++ rs

      let reply = show vimTypeReply
      liftIO $ do
        logDebug $ "replying " ++ reply
        putLine con $ S.pack $ reply
handle con unkownVersion = do
  -- handshake failure, don't accept this client version 
  liftIO $ CIO.putLine con $ 
    S.pack $ "failure: don't know how to talk to vim client version "
      ++ (show unkownVersion)

requireArg map key = do
  case M.lookup (VString key) map of
    Nothing -> fail $ "key " ++ key ++ "required"
    Just v -> fromString v
defaultArg map key default' = do
  case M.lookup (VString key) map of
    Nothing -> default'
    Just v -> fromString v
lookupAndRead dict key = do
    s <- fromString =<< M.lookup (VString key) dict
    case readEither s of
      Right x -> return x
      Left e -> Nothing
lookupAndReadFail :: (Read r) => M.Map VimType VimType -> String -> ScionM r
lookupAndReadFail dict key =
    maybe (fail $ "failed reading key " ++ key) return $ lookupAndRead dict key

-- TODO catch failures and send them as error to the client
--handleScionException  (TODO)
handleFailure :: ScionM VimType -> ScionM VimType
-- TODO narrow Exception type, check implementation
handleFailure f = do
  rep <- f
  -- success, put everything into the key "result" 
  return $ toVim [("result" , rep)]
-- handleFailure = ghandle (\(Exception e) -> return $ toVim [("error", show e)] )

------------------------------------------------------------------------------
-- implementation vim commands, also see Scion.Server.Commands

cmdConnectionInfo = VimCommand "cmdConnectionInfo" $ \map' -> do
  return $ toVim [ ("version", scionVersion),
                   ("pid", 0)]

cmdOpenCabalProject = VimCommand "cmdOpenCabalProject" $ \map' -> do
  root_dir <- requireArg map' "root_dir" 
  dist_dir <- requireArg map' "dist_dir"
  case M.lookup (VString "extra_args") map' of
    Just (VList list) -> do
      extra_args' <- mapM fromString list
      configureCabalProject root_dir dist_dir extra_args'
      preprocessPackage dist_dir
      liftM ( toVim . display . PD.package) currentCabalPackage
    Just x  -> fail $ "key extra_args: list expected, got " ++ (show x)
    Nothing -> fail $ "no arguments given!"

cmdLoadComponent = VimCommand "cmdLoadComponent" $ \map' -> do
  --  component is either "library" or "executable:name"
  component <- requireArg map' "component"
  let comp = if component == "library"
                then Library
                else let (_,_:b) = break (== ':') component in Executable b
  liftM toVim $ loadComponent comp

cmdListSupportedLanguages = VimCommand "cmdListSupportedLanguages" $ \map' -> do
  return $ toVim $ supportedLanguages

cmdListSupportedPragmas = VimCommand "cmdListSupportedPragmas" $ \map' -> do
  return $ toVim $ supportedPragmas

cmdListSupportedFlags = VimCommand "cmdListSupportedFlags" $ \map' -> do
  return $ toVim $ nub $ allFlags

cmdListRdrNamesInScope = VimCommand "cmdListRdrNamesInScope" $ \map' -> do
  rdr_names <- getNamesInScope
  return $ toVim $ map (O.showSDoc . O.ppr) rdr_names

cmdListExposedModules = VimCommand "cmdListExposedModules" $ \map' -> do
  mod_names <- allExposedModules
  return $ toVim $ map (O.showSDoc . O.ppr) mod_names

cmdSetGHCVerbosity = VimCommand "cmdSetGHCVerbosity" $ \map' -> do
  lvl <- lookupAndReadFail map' "lvl"
  liftM toVim $ setGHCVerbosity lvl

cmdBackgroundTypecheckFile = VimCommand "cmdBackgroundTypecheckFile" $ \map' -> do
  file <- requireArg map' "file"
  liftM (toVim . (\(a, b) -> [("inProject", toVim a),("compilationResult", toVim b)])) $
    backgroundTypecheckFile file

cmdForceUnload = VimCommand "cmdForceUnload" $ \map' -> do
  liftM toVim $ unload

cmdAddCmdLineFlag = VimCommand "cmdAddCmdLineFlag" $ \map' -> do
  add <- requireArg map' "add"
  addCmdLineFlags [add]
  return $ toVim ()

cmdThingAtPoint = VimCommand "cmdThingAtPoint" $ \map' -> do
  file <- requireArg map' "file"
  line <- lookupAndReadFail map' "line"
  col <- lookupAndReadFail map' "col"
  liftM toVim $ cmd file line col
  where
    -- TODO remove this code duplication ! 
    cmd fname line col = do
      let loc = srcLocSpan $ mkSrcLoc (fsLit fname) line col
      tc_res <- gets bgTcCache
      case tc_res of
        Just (Typechecked tcm) -> do
            --let Just (src, _, _, _, _) = renamedSource tcm
            let src = typecheckedSource tcm
            --let in_range = const True
            let in_range = overlaps loc
            let r = findHsThing in_range src
            --return (Just (O.showSDoc (O.ppr $ S.toList r)))
            unqual <- unqualifiedForModule tcm
            case pathToDeepest r of
              Nothing -> return (Just "no info")
              Just (x,xs) ->
                --return $ Just (O.showSDoc (O.ppr x O.$$ O.ppr xs))
                case typeOf (x,xs) of
                  Just t ->
                      return $ Just $ O.showSDocForUser unqual
                        (prettyResult x O.<+> O.dcolon O.<+> 
                          pprTypeForUser True t)
                  _ -> return (Just (O.showSDocDebug (O.ppr x O.$$ O.ppr xs )))
        _ -> return Nothing

-- cmdDumpSources = VimCommand "cmdDumpSources" $ \map -> do
--   liftM toVim $ do
--     tc_res <- gets bgTcCache
--     case tc_res of
--       Just (Typechecked tcm) -> do
--         let Just (rn, _, _, _, _) = renamedSource tcm
--         let tc = typecheckedSource tcm
--         liftIO $ putStrLn $ O.showSDocDump $ O.ppr rn
--         liftIO $ putStrLn $ showData TypeChecker 2 tc
--         return ()
--       _ -> return ()

-- ========== passing data is done using serialized vim types : ======

data VimType = VList [VimType]
               | VDict (M.Map VimType VimType)
               | VInt Int
               | VString String
  deriving (Eq, Ord)


class ToVimType a where
  toVim :: a -> VimType 

instance Show VimType where
  show (VList l) = '[':intercalate "," (map show l) ++ "]"
  show (VDict d) = '{':intercalate "," [ (show k) ++ ":" ++ (show v)  |(k,v) <- M.toList d ] ++ "}"
  show (VInt i) = show i
  show (VString s) = show s

instance ToVimType VimType where toVim = id -- only for convinience 
instance ToVimType Int    where toVim = VInt
instance ToVimType String where toVim = VString
listToVim :: (ToVimType a) => [a] -> VimType
listToVim = VList . map toVim
instance (ToVimType b) => ToVimType [b] where
  toVim = listToVim
instance (ToVimType a, ToVimType b) => ToVimType [(a,b)] where
  toVim = VDict . M.fromList . map (\(a,b) -> (toVim a, toVim b) )
instance ToVimType () where
  toVim _ = toVim [("void", toVim True)]
instance ToVimType Bool where
  toVim b = toVim $ if b then (1::Int) else 0

instance ToVimType CompilationResult where
  toVim cr = toVim [
      ("compilationSucceeded", toVim (compilationSucceeded cr)),
      ("compilationWarnings", toVim $ concatMap errMsgToVimList ([] :: [ErrMsg])),
      ("compilationErrors", toVim $ concatMap errMsgToVimList ([] :: [ErrMsg])),
      ("compilationTime", toVim ( "TODO" {- (compilationTime cr-} ))
    ]
-- return list which can be passed to setqflist 
errMsgToVimList :: ErrMsg -> [VimType]
errMsgToVimList em =
    let (fst:moreLocations) = errMsgSpans em
        loc :: SrcSpan -> [(VimType, VimType)]
        loc em = 
            [ (toVim "filename", (toVim . unpackFS) ( (srcLocFile . srcSpanStart) em))
            , (toVim "lnum", toVim ( srcLocLine . srcSpanStart $ em))
            , (toVim "col", toVim ( srcLocCol . srcSpanStart $ em))
            ]

        -- ghc does print multiline messages. So add a text qf item for all
        -- trailing lines to keep them readable
        addText :: VimType -> [String] -> [VimType]
        addText (VDict map') [msg] = [VDict $ M.insert (toVim "text") (toVim msg) map']
        addText (VDict map') (msg:msgs) = addText (VDict map') [msg] ++ map (\m -> toVim [(toVim "text", toVim m)]) msgs
        -- addText _ _ = error "never executed"        
    in
      -- first location and message 
      (addText (toVim $ loc fst) $ lines $ (O.showSDoc (errMsgShortDoc em)) ++ ("\n" ++ O.showSDoc (errMsgExtraInfo em)))
      -- more error locations - when do they occur? 
      ++ map (toVim . loc) moreLocations

instance ToVimType O.SDoc where
  toVim = toVim . O.showSDoc

instance (ToVimType a) => ToVimType (Maybe a) where
  toVim (Just x) = toVim [("Just", toVim x)]
  toVim Nothing = toVim "Nothing"

vdictFromList = VDict . M.fromList

parseVim :: String -> Either ParseError VimType
parseVim s = 
  let spaces = many (oneOf " \t")
      enclosedBy st sp p = char st >> spaces >> p >>= \r -> spaces >> char sp >> return r
      parseVim' = choice [ parseInt, parseString, parseList, parseDict ]
      parseInt = liftM (VInt . read) $ many1 (oneOf $ '-':['0'..'9'])
      parseString = liftM (VString) $ choice [parseTick, parseQuot]
        where parseQuot = char '"' >> many qchar >>= \s -> char '"' >> return s
              qchar = choice [ char '\\' >> anyChar, noneOf "\\\"" ]
              parseTick = char '\'' >> many (noneOf "\'" ) >>= \s -> char '\'' >> return s
      parseList = enclosedBy '[' ']' $ liftM VList $ sepBy parseVim' ( spaces >> char ',' >> spaces )
      keyValue = parseVim' >>= \k -> spaces >> char ':' >> spaces >> parseVim' >>= \v -> return (k,v)
      parseDict = enclosedBy '{' '}' $ liftM (VDict . M.fromList) $ sepBy keyValue ( spaces >> char ',' >> spaces )
      parseDict :: CharParser () VimType
  in parse parseVim' "connection input" s

fromString (VString s) = return s
fromString r = fail $ "string expceted, but got " ++ (show r)

-- move this to Utils? The ghc library does no longer export readEither
readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S readPrec minPrec s ] of
    [x] -> Right x
    []	-> Left "Prelude.read: no parse"
    _	-> Left "Prelude.read: ambiguous parse"
