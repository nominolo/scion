module Scion.Types.Note
  ( -- * Locations
    Location, LocSource(..), mkLocation, mkNoLoc,
    locSource, isValidLoc, noLocText, viewLoc,
    locStartCol, locEndCol, locStartLine, locEndLine,
    -- ** Absolute FilePaths
    AbsFilePath(toFilePath), mkAbsFilePath,
    -- * Notes
    Note(..), NoteKind(..), Notes, hasErrors
    -- ** Converting from GHC Notes
  )
where

import           Control.Applicative
import           Data.Binary
import qualified Data.MultiSet as MS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           System.FilePath

-- | A note from the compiler or some other tool.
data Note = Note
  { noteKind :: NoteKind
  , noteLoc :: Location
  , noteMessage :: T.Text
  } deriving (Eq, Ord, Show)

instance Binary Note where
  put (Note knd loc msg) = put knd >> put loc >> put (T.encodeUtf8 msg)
  get = Note <$> get <*> get <*> (T.decodeUtf8 <$> get)

-- | Classifies the kind (or severity) of a note.
data NoteKind
  = ErrorNote
  | WarningNote
  | InfoNote
  | OtherNote
  deriving (Eq, Ord, Show, Enum)

instance Binary NoteKind where
  put nk = putWord8 (fromIntegral (fromEnum nk))
  get = toEnum . fromIntegral <$> getWord8

type Notes = MS.MultiSet Note

hasErrors :: Notes -> Bool
hasErrors notes =
  not $ null [ () | Note{ noteKind = ErrorNote } <- MS.toList notes ]

-- | Represents a 'FilePath' which we know is absolute.
--
-- Since relative 'FilePath's depend on the a current working directory we
-- normalise all paths to absolute paths.  Use 'mkAbsFilePath' to create
-- absolute file paths.
newtype AbsFilePath = AFP { toFilePath :: FilePath } deriving (Eq, Ord)

instance Binary AbsFilePath where
  put (AFP fp) = put fp
  get = AFP <$> get

instance Show AbsFilePath where show (AFP s) = show s

-- | Create an absolute file path given a base directory.
--
-- Throws an error if the first argument is not an absolute path.
mkAbsFilePath :: FilePath -- ^ base directory (must be absolute)
              -> FilePath -- ^ absolute or relative 
              -> AbsFilePath
mkAbsFilePath baseDir dir
  | isAbsolute baseDir = AFP $ normalise $ baseDir </> dir
  | otherwise =
      error "mkAbsFilePath: first argument must be an absolute path"

-- | Scion's type for source code locations (regions).
--
-- We use a custom location type for two reasons:
--
--  1. We enforce the invariant that the file path of the location is an
--     absolute path.
--
--  2. Independent evolution from the GHC API.
--
-- To save space, the 'Location' type is kept abstract and uses special
-- cases for notes that span only one line or are only one character wide.
-- Use 'mkLocation' and 'viewLoc' as well as the respective accessor
-- functions to construct and destruct nodes.
--
-- If no reasonable location info can be given, use the 'mkNoLoc'
-- function, but be careful not to call 'viewLoc' or any other
-- accessor function on such a 'Location'.
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

instance Binary Location where
  put (LocNone msg) = putWord8 1 >> put msg
  put loc | (src, l1, c1, l2, c2) <- viewLoc loc =
    putWord8 2 >> put src >> put l1 >> put c1 >> put l2 >> put c2
  get = do 
    tag <- getWord8
    case tag of
      1 -> LocNone <$> get
      2 -> mkLocation <$> get <*> get <*> get <*> get <*> get
      _ -> fail "Binary Location get: tag error"

-- | The \"source\" of a location.
data LocSource
  = FileSrc AbsFilePath
  -- ^ The location refers to a position in a file.
  | OtherSrc String
  -- ^ The location refers to something else, e.g., the command line, or
  -- stdin.
  deriving (Eq, Ord, Show)

instance Binary LocSource where
  put (FileSrc fp) = putWord8 1 >> put fp
  put (OtherSrc s) = putWord8 2 >> put s
  get = do tag <- getWord8
           case tag of
             1 -> FileSrc <$> get
             2 -> OtherSrc <$> get
             _ -> fail "Binary LocSource get: tag error"

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

-- | Construct a source location that does not specify a region.  The
-- argument can be used to give some hint as to why there is no location
-- available.  (E.g., \"File not found\").
mkNoLoc :: String -> Location
mkNoLoc msg = LocNone msg

-- | Test whether a location is valid, i.e., not constructed with 'mkNoLoc'.
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
-- | View on a (valid) location.
--
-- It holds the property:
--
-- > prop_viewLoc_mkLoc s l0 c0 l1 c1 =
-- >     viewLoc (mkLocation s l0 c0 l1 c1) == (s, l0, c0, l1, c1)
--
viewLoc :: Location
        -> (LocSource, Int, Int, Int, Int)
           -- ^ source, start line, start column, end line, end column.
viewLoc l = (locSource l, locStartLine l, locStartCol l,
             locEndLine l, locEndCol l)

-- | Comparison function for two 'Location's.
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

-- | Lexicographic composition two orderings.  Compare using the first
-- ordering, use the second to break ties.
thenCmp :: Ordering -> Ordering -> Ordering
thenCmp EQ x = x
thenCmp x _  = x
{-# INLINE thenCmp #-}

