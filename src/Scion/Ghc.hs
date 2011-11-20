{-# LANGUAGE CPP, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Scion.Ghc
  ( -- * Converting from GHC error messages
    ghcSpanToLocation, ghcErrMsgToNote, ghcWarnMsgToNote,
    ghcMessagesToNotes, scionColToGhcCol, fromGhcModSummary
  )
where

import           Scion.Types.Note
import           Scion.Types.Session
import           Scion.Utils.Convert

import qualified ErrUtils as Ghc ( ErrMsg(..), WarnMsg, Messages )
import qualified SrcLoc as Ghc
import qualified HscTypes as Ghc
import qualified Module as Ghc
import qualified GHC as Ghc
import qualified FastString as Ghc ( unpackFS )
import qualified Outputable as Ghc ( showSDoc, ppr, showSDocForUser )
import qualified Bag ( bagToList )
import qualified Data.MultiSet as MS
import qualified Data.Text as T

import           Data.String ( fromString )
import           System.FilePath.Canonical

-- * Converting from Ghc types.

-- | Convert a 'Ghc.SrcSpan' to a 'Location'.
--
-- The first argument is used to normalise relative source locations to an
-- absolute file path.
ghcSpanToLocation :: FilePath -- ^ Base directory
                  -> Ghc.SrcSpan
                  -> Location
#if __GLASGOW_HASKELL__ >= 702
ghcSpanToLocation baseDir sp@(Ghc.RealSrcSpan rsp)
  | Ghc.isGoodSrcSpan sp =
      mkLocation mkLocFile
                 (Ghc.srcSpanStartLine rsp)
                 (ghcColToScionCol $ Ghc.srcSpanStartCol rsp)
                 (Ghc.srcSpanEndLine rsp)
                 (ghcColToScionCol $ Ghc.srcSpanEndCol rsp)
 where
   mkLocFile =
       case Ghc.unpackFS (Ghc.srcSpanFile rsp) of
         s@('<':_) -> OtherSrc s
         p -> FileSrc $ mkAbsFilePath baseDir p
ghcSpanToLocation _baseDir sp =
  mkNoLoc (Ghc.showSDoc (Ghc.ppr sp))
#else
ghcSpanToLocation baseDir sp
  | Ghc.isGoodSrcSpan sp =
      mkLocation mkLocFile
                 (Ghc.srcSpanStartLine sp)
                 (ghcColToScionCol $ Ghc.srcSpanStartCol sp)
                 (Ghc.srcSpanEndLine sp)
                 (ghcColToScionCol $ Ghc.srcSpanEndCol sp)
  | otherwise =
      mkNoLoc (Ghc.showSDoc (Ghc.ppr sp))
  where
    mkLocFile =
      case Ghc.unpackFS (Ghc.srcSpanFile sp) of
         s@('<':_) -> OtherSrc s
         p -> FileSrc $ mkAbsFilePath baseDir p
#endif

ghcErrMsgToNote :: FilePath -> Ghc.ErrMsg -> Note
ghcErrMsgToNote = ghcMsgToNote ErrorNote

ghcWarnMsgToNote :: FilePath -> Ghc.WarnMsg -> Note
ghcWarnMsgToNote = ghcMsgToNote WarningNote

-- Note that we don *not* include the extra info, since that information is
-- only useful in the case where we don not show the error location directly
-- in the source.
ghcMsgToNote :: NoteKind -> FilePath -> Ghc.ErrMsg -> Note
ghcMsgToNote note_kind base_dir msg =
    Note { noteLoc = ghcSpanToLocation base_dir loc
         , noteKind = note_kind
         , noteMessage = T.pack (show_msg (Ghc.errMsgShortDoc msg))
         }
  where
    loc | (s:_) <- Ghc.errMsgSpans msg = s
        | otherwise                    = Ghc.noSrcSpan
    unqual = Ghc.errMsgContext msg
    show_msg = Ghc.showSDocForUser unqual

-- | Convert 'Ghc.Messages' to 'Notes'.
--
-- This will mix warnings and errors, but you can split them back up
-- by filtering the 'Notes' based on the 'noteKind'.
ghcMessagesToNotes :: FilePath -- ^ Base path for normalising paths.
                               -- See 'mkAbsFilePath'.
                   -> Ghc.Messages -> Notes
ghcMessagesToNotes base_dir (warns, errs) =
    MS.union (map_bag2ms (ghcWarnMsgToNote base_dir) warns)
             (map_bag2ms (ghcErrMsgToNote base_dir) errs)
  where
    map_bag2ms f = MS.fromList . map f . Bag.bagToList

fromGhcModSummary :: MonadIO m => Ghc.ModSummary -> m ModuleSummary
fromGhcModSummary ms = do
  cpath <- case Ghc.ml_hs_file (Ghc.ms_location ms) of
             Just fp -> io $ canonical fp
             Nothing -> error "Module has no location"
  return $ ModuleSummary 
    { ms_module = convert (Ghc.moduleName (Ghc.ms_mod ms))
    , ms_fileType = case Ghc.ms_hsc_src ms of
         Ghc.HsSrcFile -> HaskellFile
         Ghc.HsBootFile -> HaskellBootFile
         Ghc.ExtCoreFile -> ExternalCoreFile
    , ms_imports =
         map (convert . Ghc.unLoc
                . Ghc.ideclName . Ghc.unLoc) (Ghc.ms_imps ms)
    , ms_location = cpath
    }

instance Convert Ghc.ModuleName ModuleName where
  convert m = fromString (Ghc.moduleNameString m)

instance Convert Target Ghc.Target where
  convert = targetToGhcTarget

targetToGhcTarget :: Target -> Ghc.Target
targetToGhcTarget (ModuleTarget mdl) =
  Ghc.Target { Ghc.targetId = Ghc.TargetModule mdl'
             , Ghc.targetAllowObjCode = True
             , Ghc.targetContents = Nothing
             }
 where mdl' = convert mdl -- Ghc.mkModuleName (C.display mdl)
targetToGhcTarget (FileTarget path) =
  -- TODO: make sure paths are absolute or relative to a known directory
  Ghc.Target { Ghc.targetId = Ghc.TargetFile path Nothing
             , Ghc.targetAllowObjCode = True
             , Ghc.targetContents = Nothing
             }
targetToGhcTarget (CabalTarget path) =
  Ghc.Target { Ghc.targetId = Ghc.TargetFile path Nothing
             , Ghc.targetAllowObjCode = False
             , Ghc.targetContents = Nothing
             }

instance Convert ModuleName Ghc.ModuleName where
  convert (ModuleName s) = Ghc.mkModuleName (T.unpack s)

ghcColToScionCol :: Int -> Int
#if __GLASGOW_HASKELL__ < 700
ghcColToScionCol c=c -- GHC 6.x starts at 0 for columns
#else
ghcColToScionCol c=c-1 -- GHC 7 starts at 1 for columns
#endif

scionColToGhcCol :: Int -> Int
#if __GLASGOW_HASKELL__ < 700
scionColToGhcCol c=c -- GHC 6.x starts at 0 for columns
#else
scionColToGhcCol c=c+1 -- GHC 7 starts at 1 for columns
#endif
