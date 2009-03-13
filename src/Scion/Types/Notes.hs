{-# LANGUAGE PatternGuards #-}
-- |
-- Module      : Scion.Types.Notes
-- Copyright   : (c) Thomas Schilling 2009
-- License     : BSD-style
--
-- Maintainer  : nominolo@googlemail.com
-- Stability   : experimental
-- Portability : portable
--
-- Notes, i.e., warnings, errors, etc.
--
module Scion.Types.Notes
  ( Location, LocSource(..), mkLocation, mkNoLoc
  , locSource, isValidLoc, noLocText, viewLoc
  , locStartCol, locEndCol, locStartLine, locEndLine
  , AbsFilePath, mkAbsFilePath
  , Note(..), NoteKind(..), Notes
  , ghcSpanToLocation, ghcErrMsgToNote, ghcWarnMsgToNote
  , ghcMessagesToNotes
  )
where

import qualified ErrUtils as GHC ( ErrMsg(..), WarnMsg, Messages )
import qualified SrcLoc as GHC
import qualified FastString as GHC ( unpackFS )
import qualified Outputable as GHC ( showSDoc, ppr, showSDocForUser )
import qualified Bag ( bagToList )

import qualified Data.MultiSet as MS
import System.FilePath

infixr 9 `thenCmp`

-- * Notes

-- | A note from the compiler or some other tool.
data Note
  = Note { noteKind :: NoteKind
         , noteLoc :: Location
         , noteMessage :: String
         } deriving (Eq, Ord, Show)

data NoteKind
  = ErrorNote
  | WarningNote
  | InfoNote
  | OtherNote
  deriving (Eq, Ord, Show)

type Notes = MS.MultiSet Note

-- * Absolute File Paths

newtype AbsFilePath = AFP FilePath deriving (Eq, Ord)
instance Show AbsFilePath where show (AFP s) = show s

mkAbsFilePath :: FilePath -- ^ base directory (must be absolute)
              -> FilePath -- ^ absolute or relative 
              -> AbsFilePath
mkAbsFilePath baseDir dir
  | isAbsolute baseDir = AFP $ normalise $ baseDir </> dir
  | otherwise =
      error "mkAbsFilePath: first argument must be an absolute path"

-- * Scion's 'Location' data type

-- | Scion's type for source code locations (regions).
--
-- We use a custom location type, for two reasons:
--
--  1. We enforce the invariant, that the file path of the location is an
--     absolute path.
--
--  2. Independent evolution from the GHC API.
--
data Location
  = LocOneLine { 
      locSource :: LocSource,
      locLine :: {-# UNPACK #-} !Int,
      locSCol :: {-# UNPACK #-} !Int,
      locECol :: {-# UNPACK #-} !Int
    }
  | LocMultiLine {
      locSource  :: LocSource,
      locSLine :: {-# UNPACK #-} !Int,
      locELine :: {-# UNPACK #-} !Int,
      locSCol  :: {-# UNPACK #-} !Int,
      locECol  :: {-# UNPACK #-} !Int
    }
  | LocPoint {
      locSource :: LocSource,
      locLine :: {-# UNPACK #-} !Int,
      locCol  :: {-# UNPACK #-} !Int
    }
  | LocNone { noLocText :: String }
  deriving (Eq, Show)

data LocSource
  = FileSrc AbsFilePath
  | OtherSrc String
  deriving (Eq, Ord, Show)

instance Ord Location where compare = cmpLoc

-- | Construct a source code location from start and end point.
--
-- If the start point is after the end point, they are swapped
-- automatically.
mkLocation :: LocSource
           -> Int -- ^ start line
           -> Int -- ^ start column
           -> Int -- ^ end line
           -> Int -- ^ end column
           -> Location
mkLocation file l0 c0 l1 c1
  | l0 > l1             = mkLocation file l1 c0 l0 c1
  | l0 == l1 && c0 > c1 = mkLocation file l0 c1 l1 c0
  | l0 == l1  = if c0 == c1
                  then LocPoint file l0 c0
                  else LocOneLine file l0 c0 c1
  | otherwise = LocMultiLine file l0 l1 c0 c1

-- | Construct a source location that does not specify a region.
mkNoLoc :: String -> Location
mkNoLoc msg = LocNone msg

-- | Test whether a location 
isValidLoc :: Location -> Bool
isValidLoc (LocNone _) = False
isValidLoc _           = True

noLocError :: String -> a
noLocError f = error $ f ++ ": argument must not be a noLoc"

-- | Return the start column.  Only defined on valid locations.
locStartCol :: Location -> Int
locStartCol l@LocPoint{} = locCol l
locStartCol LocNone{}  = noLocError "locStartCol"
locStartCol l = locSCol l

-- | Return the end column.  Only defined on valid locations.
locEndCol :: Location -> Int
locEndCol l@LocPoint{} = locCol l
locEndCol LocNone{}  = noLocError "locEndCol"
locEndCol l = locECol l

-- | Return the start line.  Only defined on valid locations.
locStartLine :: Location -> Int
locStartLine l@LocMultiLine{} = locSLine l
locStartLine LocNone{}  = noLocError "locStartLine"
locStartLine l = locLine l

-- | Return the end line.  Only defined on valid locations.
locEndLine :: Location -> Int
locEndLine l@LocMultiLine{} = locELine l
locEndLine LocNone{}  = noLocError "locEndLine"
locEndLine l = locLine l

{-# INLINE viewLoc #-}
viewLoc :: Location -> (LocSource, Int, Int, Int, Int)
viewLoc l = (locSource l, locStartLine l, locStartCol l,
             locEndLine l, locEndLine l)

cmpLoc :: Location -> Location -> Ordering
cmpLoc LocNone{} _ = LT
cmpLoc _ LocNone{} = GT
cmpLoc l1 l2 =
    (f1 `compare` f2) `thenCmp`
    (sl1 `compare` sl2) `thenCmp`
    (sc1 `compare` sc2) `thenCmp`
    (el1 `compare` el2) `thenCmp`
    (ec1 `compare` ec2)
 where
   (f1, sl1, sc1, el1, ec1) = viewLoc l1
   (f2, sl2, sc2, el2, ec2) = viewLoc l2

{-# INLINE thenCmp #-}
thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ x = x
thenCmp x _  = x

-- * Converting from GHC types.

-- | Convert a 'GHC.SrcSpan' to a 'Location'.
ghcSpanToLocation :: FilePath -- ^ Base directory
                  -> GHC.SrcSpan
                  -> Location
ghcSpanToLocation baseDir sp
  | GHC.isGoodSrcSpan sp =
      mkLocation mkLocFile
                 (GHC.srcSpanStartLine sp)
                 (GHC.srcSpanStartCol sp)
                 (GHC.srcSpanEndLine sp)
                 (GHC.srcSpanEndCol sp)
  | otherwise =
      mkNoLoc (GHC.showSDoc (GHC.ppr sp))
 where
   mkLocFile =
       case GHC.unpackFS (GHC.srcSpanFile sp) of
         s@('<':_) -> OtherSrc s
         p -> FileSrc $ mkAbsFilePath baseDir p

ghcErrMsgToNote :: FilePath -> GHC.ErrMsg -> Note
ghcErrMsgToNote = ghcMsgToNote ErrorNote

ghcWarnMsgToNote :: FilePath -> GHC.WarnMsg -> Note
ghcWarnMsgToNote = ghcMsgToNote WarningNote

-- Note that we don *not* include the extra info, since that information is
-- only useful in the case where we don not show the error location directly
-- in the source.
ghcMsgToNote :: NoteKind -> FilePath -> GHC.ErrMsg -> Note
ghcMsgToNote note_kind base_dir msg =
    Note { noteLoc = ghcSpanToLocation base_dir loc
         , noteKind = note_kind
         , noteMessage = show_msg (GHC.errMsgShortDoc msg)
         }
  where
    loc | (s:_) <- GHC.errMsgSpans msg = s
        | otherwise                    = GHC.noSrcSpan
    unqual = GHC.errMsgContext msg
    show_msg = GHC.showSDocForUser unqual

ghcMessagesToNotes :: FilePath -> GHC.Messages -> Notes
ghcMessagesToNotes base_dir (warns, errs) =
    MS.union (map_bag2ms (ghcWarnMsgToNote base_dir) warns)
             (map_bag2ms (ghcErrMsgToNote base_dir) errs)
  where
    map_bag2ms f = MS.fromList . map f . Bag.bagToList
