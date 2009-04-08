{-# LANGUAGE CPP #-}
-- |
-- Module      : Scion.Inspect.DefinitionSite
-- Copyright   : (c) Thomas Schilling 2009
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Finding the definition site of an identifier.
--
-- This module analyses Haskell code to find the definition sites of
-- identifiers within.
--
-- TODO: collect type info as well?
module Scion.Inspect.DefinitionSite where

import Scion.Types.Notes

import GHC
import Bag ( bagToList )
import Name ( getOccString )

------------------------------------------------------------------------

instance Show ModuleName where
  show m = moduleNameString m

data DefSite
  = DefSite ModuleName String Location
    -- ^ The definition of the given identifier.
  | InstanceSite ModuleName String String Location
    -- ^ XXX: An instance of something
    deriving (Eq, Ord, Show)

definedNames :: (ModuleName, FilePath) -> HsGroup Name
             -> [DefSite]
definedNames srcmod hsgroup =
  let vals = case hs_valds hsgroup of
               ValBindsOut nest _sigs ->
                   [ site
                       | (_rec, binds) <- nest
                       , bind <- bagToList binds
                       , site <- definedNamesHsBind srcmod bind ]
               _other -> error "definedNames: ValBindsOut expected"

      tys = [ site | ns <- map (tyClDeclNames . unLoc) (hs_tyclds hsgroup)
                   , site <- map (mkSiteOfLName srcmod) ns ]

      foreigns = concat $ map foreignBound (hs_fords hsgroup)
                 where foreignBound lfordecl =
                         case unLoc lfordecl of
                           ForeignImport n _ _ -> [mkSiteOfLName srcmod n]
                           ForeignExport { } -> []
  in vals ++ tys ++ foreigns

definedNamesHsBind :: (ModuleName, FilePath) -> LHsBind Name -> [DefSite]
definedNamesHsBind srcmod lbind =
  case unLoc lbind of
    FunBind { fun_id = name } -> [mkSite name]
    PatBind { pat_lhs = lhs } -> definedNamesPat srcmod lhs
    VarBind { var_id = name } ->
        [DefSite (fst srcmod) (getOccString name) (theLoc (snd srcmod) lbind)]
    AbsBinds { } -> [] -- nothing interesting in a type abstraction
 where
   mkSite = mkSiteOfLName srcmod

definedNamesPat :: (ModuleName, FilePath) -> LPat Name -> [DefSite]
definedNamesPat srcmod lhs = go lhs []
  where
    mkSite = mkSiteOfLName srcmod
    go lpat acc =
      let loc = theLoc (snd srcmod) lpat
          lid name = DefSite (fst srcmod) (getOccString name) loc
      in case unLoc lpat of
           WildPat _        -> acc
           VarPat name      -> lid name : acc
           VarPatOut name _ -> lid name : acc -- XXX need help here
           LazyPat p        -> go p acc
           AsPat name p     -> go p (mkSite name : acc)
           ParPat p         -> go p acc
           BangPat p        -> go p acc
           ListPat ps _     -> foldr go acc ps
           TuplePat ps _ _  -> foldr go acc ps
           PArrPat ps _     -> foldr go acc ps
           ConPatIn _ conargs -> conArgs conargs acc
           ConPatOut _ _ _ _ conargs _ -> conArgs conargs acc
           LitPat _         -> acc
#if __GLASGOW_HASKELL__ > 608
           NPat _ _ _       -> acc -- form of literal pattern?
#else
           NPat _ _ _ _     -> acc -- form of literal pattern?
#endif
           NPlusKPat name _ _ _ -> mkSite name : acc
           TypePat _        -> acc -- XXX need help here
           SigPatIn p _     -> go p acc
           SigPatOut p _    -> go p acc
           _ -> error "definedNamesPat"

    conArgs (PrefixCon ps) acc = foldr go acc ps
    conArgs (RecCon (HsRecFields { rec_flds = flds })) acc 
             = foldr (\f acc' -> go (hsRecFieldArg f) acc') acc flds
    conArgs (InfixCon p1 p2) acc = go p1 $ go p2 acc

theLoc :: FilePath -> Located a -> Location
theLoc base_path l = ghcSpanToLocation base_path (getLoc l)

mkSiteOfLName :: (ModuleName, FilePath) -> Located Name -> DefSite
mkSiteOfLName (srcmod, base_path) name =
    DefSite srcmod (getOccString $ unLoc name) (theLoc base_path name)
