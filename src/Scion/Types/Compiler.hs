module Scion.Types.Compiler where

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get()
import           Data.String ( IsString(fromString) )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype Extension = Ext { extensionName :: T.Text }
  deriving (Eq, Ord)

instance Show Extension where
  show = T.unpack . extensionName

instance Binary Extension where
  put (Ext nm) = put (T.encodeUtf8 nm)
  get = Ext . T.decodeUtf8 <$> get

instance IsString Extension where
  fromString s = Ext (T.pack s)
