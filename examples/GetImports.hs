module Main where

import Scion
import MonadUtils ( liftIO )

main = runScion $ do
         liftIO $ print "hello"
         setWorkingDir "./tests"
         
