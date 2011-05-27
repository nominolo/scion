{-# LANGUAGE TemplateHaskell #-}
module ProtocolJSON where
import Scion.Types.Note
import qualified Data.MultiSet as MS
import Scion.Types.Compiler ( Extension(..), extensionName )
import Text.JSON
import ServerTypes
import Data.DeriveTH
import Scion.Types.Session hiding ( catch )
import qualified Data.Text as T

-- all those lines implement JSON encoding for both main types:
-- ServerCommand and ServerResponse

instance JSON T.Text where
  readJSON  = fmap T.pack . readJSON
  showJSON  = showJSON . T.unpack

$(derive makeJSON ''Component)
$(derive makeJSON ''SessionConfig)
$(derive makeJSON ''ServerCommand)

instance JSON Extension where
  readJSON  = fmap Ext . readJSON
  showJSON  = showJSON . extensionName

instance JSON SessionId where
  readJSON  = fmap SessionId . readJSON
  showJSON  (SessionId i) = showJSON i

instance JSON ModuleName where
  readJSON  = fmap ModuleName . readJSON
  showJSON  (ModuleName i) = showJSON i

instance (JSON a, Ord a) => JSON (MS.MultiSet a) where
  readJSON  = fmap MS.fromMap . readJSON
  showJSON  = showJSON . MS.toMap

instance JSON NoteKind where
  showJSON ErrorNote   = JSString (toJSString "error")
  showJSON WarningNote = JSString (toJSString "warning")
  showJSON InfoNote    = JSString (toJSString "info")
  showJSON OtherNote   = JSString (toJSString "other")
  readJSON (JSString s) =
      case lookup (fromJSString s) 
               [("error", ErrorNote), ("warning", WarningNote)
               ,("info", InfoNote), ("other", OtherNote)]
      of Just x -> return x
         Nothing -> fail "note-kind"
  readJSON _ = fail "note-kind"

instance JSON Location where
  showJSON loc | not (isValidLoc loc) =
    makeObject [("no-location", str (noLocText loc))]
  showJSON loc | (src, l0, c0, l1, c1) <- viewLoc loc =
    makeObject [case src of
                  FileSrc f -> ("file", str (toFilePath f))
                  OtherSrc s -> ("other", str s)
               ,("region", JSArray (map showJSON [l0,c0,l1,c1]))]
  readJSON (JSObject obj) =  error "unexpected usage of readJSON of Location"
    {-
    do
    src <- (do JSString f <- lookupKey obj "file"
               return (FileSrc (mkAbsFilePath "/" (fromJSString f))))
           <|>
           (do JSString s <- lookupKey obj "other"
               return (OtherSrc (fromJSString s)))
    JSArray ls <- lookupKey obj "region"
    case mapM readJSON ls of
      Ok [l0,c0,l1,c1] -> return (mkLocation src l0 c0 l1 c1)
      _ -> fail "region"
    -}
  readJSON _ = fail "location"

instance JSON Note where
  showJSON (Note note_kind loc msg) =
    makeObject [("kind", showJSON note_kind)
               ,("location", showJSON loc)
               ,("message", JSString ((toJSString . T.unpack) msg))]
  {-
  readJSON (JSObject obj) = do
    note_kind <- readJSON =<< lookupKey obj "kind"
    loc <- readJSON =<< lookupKey obj "location"
    JSString s <- lookupKey obj "message"
    return (Note note_kind loc (fromJSString s))
  -}
  readJSON _ = fail "note"

str :: String -> JSValue
str = JSString . toJSString

lookupKey :: JSON a => JSObject JSValue -> String -> Result a
lookupKey = flip valFromObj

makeObject :: [(String, JSValue)] -> JSValue
makeObject = makeObj

$(derive makeJSON ''HsFileType)
$(derive makeJSON ''ModuleSummary)
$(derive makeJSON ''ServerResponse)
