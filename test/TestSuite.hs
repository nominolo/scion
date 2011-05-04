module Main ( main ) where

import Scion.Types.Monad
import Scion.Types.Session
import Scion.Session
import Scion.Cabal

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

cabal_config001 p =
  CabalConfig
    { sc_name = "hello"
    , sc_cabalFile = p </> "tests" </> "projects" </> "hello" </> "hello.cabal"
    , sc_component = Library
    , sc_configFlags = []
    }

cabal_file001 p =
  p </> "tests" </> "projects" </> "hello" </> "hello.cabal"

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
        io $ assertBool "There should be some supported extensions." (length exts > 0),

    testCase "cabal01" $ runScion $ do
      comps <- fileComponents (cabal_file001 ".")
      io $ comps @?= [Executable "hello"],

    testCase "cabal02" $ runScion $ do
      comps <- ignoreMostErrors $ fileComponents ("./foobar.blab")
      io $ comps @?= Nothing,

    testCase "cabal10" $ runScion $ do
      withSession (cabal_config001 ".") $ \sid -> do
        notes <- sessionNotes sid
        io $ MS.size notes @?= 42  -- TODO: 
  ]
