module Main where

import GHC
import Scion
import MonadUtils ( liftIO )
import qualified StringBuffer as SB
import Outputable

import Data.Maybe

main = runScion $ do
  liftIO $ print "hello"
  setWorkingDir "../ghc/compiler"
  openCabalProject "./dist-stage2"
  setDynFlagsFromCabal Library
  setTargetsFromCabal Library
  load LoadAllTargets
  cs <- flip foldModSummaries 0 $ \n ms -> do 
    let n' = maybe 0 SB.len $ ms_hspp_buf ms
    liftIO $ print (showSDoc (ppr (ms_mod_name ms)),
                    n')
    return $! n + n'
  liftIO $ print ("total", cs)