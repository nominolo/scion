module Main ( main ) where

import Scion.Types.Monad
import Scion.Types.Session
import Scion.Session

import Data.String
import qualified Data.MultiSet as MS
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import System.FilePath ( (</>) )

main = defaultMain tests

file_config001 p =
  FileConfig (p </> "tests" </> "projects" </> "file001.hs") []

file_config002 p =
  FileConfig (p </> "tests" </> "projects" </> "file001.hs") ["-Wall"]

tests =
  [ testCase "ping" $ runScion $ do
      withSession (file_config001 ".") $ \sid -> do
        ok <- ping sid
        io $ assertBool "Answer to ping must be pong." ok,
    testCase "notes" $ runScion $ do
      withSession (file_config002 ".") $ \sid -> do
        notes <- sessionNotes sid
        io $ MS.size notes @?= 2,
    testCase "exts" $ runScion $ do
      withSession (file_config001 ".") $ \sid -> do
        exts <- supportedLanguagesAndExtensions
        io $ assertBool "There should be some supported extensions." (length exts > 0)
  ]
