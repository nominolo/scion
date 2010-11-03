-- | The backend abstraction (well, the beginnings of it.)
{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Scion.Backend 
  ( supportedLanguages, supportedOptions, optionsPragma,
    supportedPragmas,
    ModuleName, mkModuleName, moduleNameText,
    allExposedModuleNames,
  )
where

import Scion.Types

import qualified DynFlags as Ghc
import qualified GHC as Ghc
import qualified Packages as Ghc
import qualified UniqFM as Ghc
--import qualified Module as Ghc
import qualified Distribution.InstalledPackageInfo as PInfo

import qualified Data.Text            as T
import Data.List ( nub, sort )

-- | Sorted list of all supported @LANGUAGE@ pragmas.
supportedLanguages :: [T.Text]
supportedLanguages = sort $ map T.pack Ghc.supportedLanguages

-- | Sorted list of all flags allowed in the @OPTIONS@ pragma.
supportedOptions :: [T.Text]
supportedOptions = sort $ map T.pack (nub Ghc.allFlags)

-- | The compiler-specific @OPTIONS@ pragma.  (e.g., @OPTIONS_GHC@)
optionsPragma :: T.Text
optionsPragma = "OPTIONS_GHC"

-- | List of supported pragmas.
supportedPragmas :: [T.Text]
supportedPragmas = sort $
  [ "OPTIONS_GHC", "LANGUAGE", "INCLUDE", "WARNING", "DEPRECATED"
  , "INLINE", "NOINLINE", "RULES", "SPECIALIZE", "UNPACK", "SOURCE"
  , "SCC"
  -- , "LINE" -- only used by code generators, still include?
  ]

-- -------------------------------------------------------------------

-- TODO: Move this to a better place?

-- | A module name (as used in @import@ lists).  E.g., @Control.Monad@.
newtype ModuleName = ModuleName { moduleNameText :: T.Text }
  deriving (Eq, Ord)

instance Show ModuleName where
  show (ModuleName txt) = T.unpack txt

mkModuleName :: T.Text -> ModuleName
mkModuleName = ModuleName

-- | Return all exposed modules of the current session's package DBs.
-- Note that this does /not/ include packages from the current
-- session.
--
-- The provides a crude yet simple basis for completing @import@
-- directives.
allExposedModuleNames :: ScionM [ModuleName]
allExposedModuleNames = do
   dflags <- Ghc.getSessionDynFlags
   let pkg_db = Ghc.pkgIdMap (Ghc.pkgState dflags)
   return $ nub
          . map (ModuleName . T.pack . Ghc.moduleNameString)
          . concatMap PInfo.exposedModules 
          -- . filter exposed
          $ Ghc.eltsUFM pkg_db

