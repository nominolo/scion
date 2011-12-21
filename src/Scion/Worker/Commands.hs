{-# LANGUAGE CPP, ScopedTypeVariables #-}
module Scion.Worker.Commands where

import Scion.Types.Compiler
import Scion.Types.Worker
import Scion.Types.Commands as C
import Scion.Types.Session
import Scion.Ghc( fromGhcModSummary )

import qualified GHC as Ghc
import qualified DynFlags as Ghc
import qualified HscTypes as Ghc
import qualified ErrUtils as Ghc
import Bag ( unionBags, emptyBag )

import Control.Applicative
import Data.List ( sort )
import Data.Time.Clock
import Data.IORef
import Data.Monoid
import Data.String
import qualified Data.MultiSet as MS

type KeepGoing = Bool


handleRequest :: Command -> Worker (Answer, KeepGoing)
handleRequest cmd = case cmd of
  Ping ->
    return (Pong, True)
  SetConfig _ ->
    return (C.Error "Can only set config once.", True)
  Quit ->
    return (Quitting, False)
  Reload -> do
    ans <- CompResult <$> load Ghc.LoadAllTargets
                      <*> moduleGraph
    return (ans, True)
  Extensions ->
    return (AvailExtensions supportedLanguages, True)

supportedLanguages :: [Extension]
#if __GLASGOW_HASKELL__ >= 700
supportedLanguages = sort $ map fromString Ghc.supportedLanguagesAndExtensions
#else
supportedLanguages = sort $ map fromString Ghc.supportedLanguages
#endif

-- -------------------------------------------------------------------

-- | Run a computation and measure its run time.
--
-- Normally, measures the time until the action returns.  The argument
-- to the callback can be used to stop the timer before the callback
-- returns.  Sample usage:
--
-- > withMeasuredTime $ \stop_timer ->
-- >   callFooBar
-- >   if someCondition then stop_time else doSomeMoreWork
--
withMeasuredTime :: MonadIO m =>
                    (m () -> m a)
                 -> m (a, NominalDiffTime)
withMeasuredTime kont = do
  time <- liftIO $ newIORef . Left =<< getCurrentTime
  a <- kont (stop_time time)
  stop_time time
  Right time_diff <- liftIO $ readIORef time
  return (a, time_diff)
 where
   stop_time time = liftIO $ do
     t <- readIORef time
     case t of
       Left start_time -> do
         end_time <- getCurrentTime
         writeIORef time (Right (diffUTCTime end_time start_time))
       Right _time_diff -> return ()


data Messages = Messages Ghc.WarningMessages Ghc.ErrorMessages

instance Monoid Messages where
  mempty = Messages emptyBag emptyBag
  Messages ws1 es1 `mappend` Messages ws2 es2 =
    Messages (ws1 `unionBags` ws2) (es1 `unionBags` es2)


-- | Wrapper for 'Ghc.load'.
load :: Ghc.LoadHowMuch -> Worker CompilationResult
load how_much = do
  --msgs <- liftIO $ newIORef (mempty :: Messages)
  _ <- getAndClearNewNotes

  (res, time_diff)
    <- withMeasuredTime $ \_stop_timer -> do
         Ghc.load how_much --WithLogger (my_logger msgs) how_much
           `gcatch` (\(e :: Ghc.SourceError) -> do
#if __GLASGOW_HASKELL__ >= 702
                      Ghc.printException e
#else
                      Ghc.printExceptionAndWarnings e
#endif
                      return Ghc.Failed
                    ) --handle_error msgs e)

{-
  base_dir <- liftIO $ getCurrentDirectory
  Messages warns errs <- liftIO $ readIORef msgs
  let notes = ghcMessagesToNotes base_dir (warns, errs)
-}
  new_notes <- getAndClearNewNotes
  let notes = MS.fromList new_notes

  let comp_rslt =
        case res of
          Ghc.Succeeded -> CompilationResult True notes time_diff
          Ghc.Failed -> CompilationResult False notes time_diff

  return comp_rslt

moduleGraph :: Worker [ModuleSummary]
moduleGraph = do
  mapM fromGhcModSummary =<< Ghc.getModuleGraph
