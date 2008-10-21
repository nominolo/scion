module Main where

import WriteDotGraph

import GHC
import Scion
import Scion.Inspect
import Bag ( bagToList )
import Outputable
--import TcRnTypes
import HscTypes
import SrcLoc

import GHC.Uniplate.Instances

import Control.Monad
import System.Exit

main = runScion $ do
  setWorkingDir "../ghc/compiler" -- "../mtl-1.1.0.2" --"../ghc/compiler"
  openCabalProject "./dist-stage2" --"dist" --"./dist-stage2"
  setDynFlagsFromCabal Library
  setTargetsFromCabal Library
  addCmdLineFlags ["-DSTAGE=2"]
  handleSourceError print_error_and_exit $ do
    mss <- modulesInDepOrder
    io $ print (length mss)
    deps <- forM mss $ \ms -> do
      --mod <- loadModule =<< typecheckModule =<< parseModule ms
      --clearWarnings
      return ( moduleNameString (ms_mod_name ms) 
                 ++ hscSourceString (ms_hsc_src ms),
               map (moduleNameString . unLoc) (ms_srcimps ms)
                ++ map (moduleNameString . unLoc) (ms_imps ms) )
    io $ writeFile "deps.dot" $ 
       writeDotGraph [ (Lit mod, Lit imp) 
                         | (mod, imps) <- deps
                         , imp <- imps ]

{-
      let binds = bagToList (typecheckedSource mod)
      let Just rn@(grp, _, _, _, _) = renamedSource mod
      io $ putStrLn $ moduleNameString (ms_mod_name ms)
--       io $ putStrLn $ showSDoc $ nest 4 $ ppr $ typeDecls mod
--       io $ putStrLn $ showSDoc $ nest 4 $ ppr $ classDecls rn
      pp $ ppr $ tcg_dus (fst (tm_internals mod))
      return ()
-}
print_error_and_exit err = do
  printExceptionAndWarnings err
  io $ exitWith (ExitFailure 1)

pp :: SDoc -> ScionM ()
pp = io . putStrLn . showSDoc

newtype Lit = Lit String deriving Eq
instance Show Lit where show (Lit s) = s
