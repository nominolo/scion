module ServerTypes where
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


import qualified Data.Text as T

-- Extend this to support new commands
data ServerCommand
  = ConnectionInfo
  | ListSupportedLanguages
  | QuitServer
  | ListAvailConfigs T.Text -- cabal file
  | CreateSession SessionConfig
  | FileModified T.Text
  deriving Show

data ServerResponse
  = RConnectionInfo Int -- protocol version
  | RSupportedLanguages [Extension]
  | RQuitting
  | RFileConfigs [SessionConfig]
  | RSessionCreated SessionId Bool Notes [ModuleSummary]
  | RFileModifiedResult Bool Notes

