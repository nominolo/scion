module Main where

import Scion
import Scion.Server.Options
import Scion.Server.Emacs

main = do
    opts <- readOptions
    runScion $ runServer opts
