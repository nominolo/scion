module Scion.Types.Commands where

import Scion.Types.Compiler
import Scion.Types.Session

import           Control.Applicative
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

data Command 
  = Ping
  | SetConfig SessionConfig
  | Quit
  | Reload
  | Extensions
  deriving Show

data Answer
  = Pong
  | CompResult CompilationResult [ModuleSummary]
  | Error String
  | Quitting
  | AvailExtensions [Extension]
  deriving Show

instance Binary Command where
  put Ping             = putWord16le 1
  put (SetConfig cfg)  = putWord16le 2 >> put cfg
  put Quit             = putWord16le 3
  put Reload           = putWord16le 4
  put Extensions       = putWord16le 5

  get = do
    tag <- getWord16le
    case tag of
      1 -> pure Ping
      2 -> SetConfig <$> get
      3 -> pure Quit
      4 -> pure Reload
      5 -> pure Extensions
      _ -> fail "Binary Command get: tag error"

instance Binary Answer where
  put Pong             = putWord16le 1
  put (CompResult r g) = putWord16le 2 >> put r >> put g
  put (Error msg)      = putWord16le 3 >> put msg
  put Quitting         = putWord16le 4
  put (AvailExtensions exts) = putWord16le 5 >> put exts
  
  get = do
    tag <- getWord16le
    case tag of
      1 -> pure Pong
      2 -> CompResult <$> get <*> get
      3 -> Error <$> get
      4 -> pure Quitting
      5 -> AvailExtensions <$> get
      _ -> fail "Binary Answer get: tag error"
