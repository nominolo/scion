module Main where

import GHC
import Scion
import MonadUtils ( liftIO )
import qualified StringBuffer as SB
import Outputable
import HscTypes

import Data.Maybe
import Control.Monad

main = runScion $ do
  liftIO $ print "hello"
  setWorkingDir "../ghc/compiler"
  openCabalProject "./dist-stage2"
  setDynFlagsFromCabal Library
  setTargetsFromCabal Library
  --load LoadAllTargets
  flip foldModSummaries () $ \n ms -> do 
    io $ putStrLn $ 
       showSDoc $ hang (ppr (ms_mod ms) <+> text (hscSourceString (ms_hsc_src ms)))
                       4 (ppr (ms_imps ms))
    return ()
  --liftIO $ print ("total", cs)