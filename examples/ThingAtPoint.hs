import GHC
import Scion
import Scion.Utils
import Outputable

main = runScion $ do
  addTarget =<< guessTarget "./tests/Test001.hs" Nothing
  load LoadAllTargets
  [ms] <- modulesInDepOrder
  mod <- typecheckModule =<< parseModule ms
  let Just (grp, _, _, _, _) = renamedSource mod
  --io $ putStrLn $ showSDoc $ ppr $ grp
  let tyclds = thingsAroundPoint (8,5) (hs_tyclds grp)
  io $ putStrLn $ showSDoc $ ppr $ tyclds
  return ()

