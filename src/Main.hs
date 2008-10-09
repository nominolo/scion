import GHC
import GHC.Paths ( libdir )
import MonadUtils ( liftIO, MonadIO )
import DynFlags
import Packages
import Module
import Outputable
import Exception

main = 
    defaultErrorHandler defaultDynFlags $ do
      pr "Starting"
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        (dflags', pkgIds) <- liftIO $ initPackages dflags
        setSessionDynFlags dflags'{ hscTarget = HscInterpreted }
        pr "Session set up"
        let prelude_mod = mkModule (stringToPackageId "base")
                                   (mkModuleName "Prelude")
        setContext [] [prelude_mod]
        pr "Context set"

        let stmt = "let n = 2 + 'a'"
        handleSourceError (\err -> do
                             pr "Failed to compile stmt"
                             printExceptionAndWarnings err) $ do
          rslt <- runStmt stmt RunToCompletion
          pr $ "Result is: " ++ case rslt of
                                  RunOk names -> "OK: " ++ showSDoc (ppr names)
                                  _ -> "Failed"

        return ()

pr :: (MonadIO m) => String -> m ()
pr = liftIO . putStrLn