module Main where

import Scion.Worker.Main ( workerMain )

main :: IO ()
main = workerMain 42
--main = soloWorkerMain