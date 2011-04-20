module Main where

import Scion.Types.Monad
import Scion.Types.Session
import Scion.Session

import qualified Data.MultiSet as MS

main = runScion $ do
  sid <- createSession (FileConfig "tests/projects/file001.hs" ["-Wall"])
  ok <- ping sid
  io . print . MS.size =<< sessionNotes sid
  io (print ok)
