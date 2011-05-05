module Main ( main ) where

import Scion.Types.Monad
import Scion.Types.Session
import Scion.Session
import Scion.Cabal

import Control.Concurrent ( threadDelay )
import Data.String
import qualified Data.MultiSet as MS
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import System.Directory
import System.FilePath ( (</>) )
import System.IO


main = defaultMain tests

file_config001 p =
  FileConfig (p </> "tests" </> "projects" </> "file001.hs") []

file_config002 p =
  FileConfig (p </> "tests" </> "projects" </> "file001.hs") ["-Wall"]

cabal_config001 p =
  CabalConfig
    { sc_name = "hello"
    , sc_cabalFile = p </> "tests" </> "projects" </> "hello" </> "hello.cabal"
    , sc_component = Executable "hello"
    , sc_configFlags = []
    , sc_buildDir = Nothing
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

    testCase "cabal03" $ runScion $ do
      confs <- cabalSessionConfigs (cabal_file001 ".")
      io $ map sc_name confs @?= ["hello:hello"],

    testCase "cabal10" $ runScion $ do
      withSession (cabal_config001 ".") $ \sid -> do
        notes <- sessionNotes sid
        io $ MS.size notes @?= 0,

    testCase "recomp01" test_recomp01 
        
  ]

-- Tests recompilation
test_recomp01 = runScion $ do
  (tmpfile, h) <- io $ do dir <- getTemporaryDirectory
                          openTempFile dir "ScionTest.hs"
  io $ hPutStr h contents0 >> hFlush h
  (withSession (FileConfig tmpfile []) $ \sid -> do
     notes <- sessionNotes sid
     io $ MS.size notes @?= 1
     io $ print notes
     io $ threadDelay 1000000 -- make sure we get a different timestamp
     io $ hSeek h AbsoluteSeek 0 >> hPutStr h contents1 >> hFlush h
     fileModified sid tmpfile
     notes2 <- sessionNotes sid
     io $ MS.size notes2 @?= 0
     io $ print notes2
   )
   `gfinally` io (hClose h)
 where
   contents0 = unlines ["module Main where",
                        "main = putStrLn ['a' 'a']"]
   contents1 = unlines ["module Main where",
                        "main = putStrLn ['a','a']"]
