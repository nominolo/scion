{-# LANGUAGE PatternGuards, CPP,
             FlexibleInstances, FlexibleContexts, MultiParamTypeClasses,
             StandaloneDeriving, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-name-shadowing #-}
-- |
-- Module      : Scion.Inspect
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Functionality to inspect Haskell programs.
--
module Scion.Inspect 
  ( module Scion.Inspect
  , module Scion.Inspect.Find
  ) where

import Scion.Utils()
import Scion.Inspect.Find

import GHC
import Bag
import Var ( varType )
import DataCon ( dataConUserType )
import Type ( tidyType )
import VarEnv ( emptyTidyEnv )

import Data.Generics.Biplate
import Data.Generics.UniplateStr hiding ( Str (..) )
import qualified Data.Generics.Str as U 
import Data.Map ( Map )
import qualified Data.Map as M
import Outputable
import GHC.SYB.Utils

#ifdef DEBUG
--import FastString
import Test.QuickCheck()
import Test.GHC.Gen()
--import Debug.Trace
--import StaticFlags ( initStaticOpts )
#endif
------------------------------------------------------------------------------

typeOfResult :: SearchResult Id -> Maybe Type
typeOfResult (FoundId i) = Just $ tidyType emptyTidyEnv $ varType i
typeOfResult (FoundCon _ c) = Just $ dataConUserType c
typeOfResult _ = Nothing

prettyResult :: OutputableBndr id => SearchResult id -> SDoc
prettyResult (FoundId i) = ppr i
prettyResult (FoundName n) = ppr n
prettyResult (FoundCon _ c) = ppr c
prettyResult r = ppr r

------------------------------------------------------------------------------

typeDecls :: TypecheckedMod m => m -> [LTyClDecl Name]
typeDecls m | Just (grp, _, _, _, _) <- renamedSource m =
    [ t | t <- hs_tyclds grp
        , isDataDecl (unLoc t) 
            || isTypeDecl (unLoc t) 
            || isSynDecl (unLoc t) ]
    -- XXX: include families?
typeDecls _ = error "typeDecls: No renamer information available."

classDecls :: RenamedSource -> [LTyClDecl Name]
classDecls (grp, _, _, _, _) =
    [ t | t <- hs_tyclds grp
        , isClassDecl (unLoc t) ]

familyDecls :: RenamedSource -> [LTyClDecl Name]
familyDecls (grp, _, _, _, _) =
    [ t | t <- hs_tyclds grp
        , isFamilyDecl (unLoc t) ]

toplevelNames :: TypecheckedMod m => m -> [Name]
toplevelNames m | Just (grp, _imps, _exps, _doc, _hmi) <- renamedSource m =
    [ n | L _ tycld <- hs_tyclds grp
        , L _ n <- tyClDeclNames tycld
    ]
   ++
    let ValBindsOut bind_grps _sigs = hs_valds grp
    in [ n | (_, binds0) <- bind_grps
           , L _ bind <- bagToList binds0
           , n <- case bind of
                    FunBind {fun_id = L _ n} -> [n]
                    PatBind {pat_lhs = L _ p} -> pat_names p
                    _ -> []
       ]
  where
    -- return names bound by pattern
    pat_names :: Pat Name -> [Name]
    pat_names pat = 
        [ n | Just n <- map pat_bind_name 
                          (trace (showData Renamer 2 (pat, universe pat)) (universe pat)) ]

    pat_bind_name :: Pat Name -> Maybe Name
    pat_bind_name (VarPat id) = Just id
    pat_bind_name (AsPat (L _ id) _) = Just id
    pat_bind_name _ = Nothing
toplevelNames _ = []

data ThingAtPoint
  = ExprThing (HsExpr Name)
  | PatThing  (Pat Name)
  | NoThing

thingAtPoint :: TypecheckedMod m => m -> ThingAtPoint
thingAtPoint _m = NoThing

------------------------------------------------------------------------------
data LocMap a
  = LocLeaf a  -- INVARIANT: not at top-level
  | LocNode (Map SrcSpan (LocMap a)) -- INVARIANT: non-overlapping, good SrcSpans


------------------------------------------------------------------------------

instance Uniplate a => Biplate a a where
  biplate = uniplate

instance Uniplate (Pat n) where
  uniplate pat = case pat of
    WildPat _         -> (Zero, \Zero -> pat)
    VarPat _          -> (Zero, \Zero -> pat)
    VarPatOut _n _     -> (Zero, \Zero -> pat) --down binds (VarPatOut n)
    LazyPat (L s p)   -> (One p, \(One p') -> LazyPat (L s p'))
    AsPat n (L s p)   -> (One p, \(One p') -> AsPat n (L s p'))
    ParPat (L s p)    -> (One p, \(One p') -> ParPat (L s p'))
    BangPat (L s p)   -> (One p, \(One p') -> BangPat (L s p'))
    ListPat ps t      -> down ps (\ps' -> ListPat ps' t)
    TuplePat ps b t   -> down ps (\ps' -> TuplePat ps' b t)
    PArrPat ps t      -> down ps (\ps' -> PArrPat ps' t)
    ConPatIn c details -> down details (ConPatIn c)
    ConPatOut dcon tvs pds binds args ty -> -- also look inside binds?
              down args (\args' -> ConPatOut dcon tvs pds binds args' ty)
    ViewPat e (L s p) t -> (One p, \(One p') -> ViewPat e (L s p') t)
    QuasiQuotePat _   -> (Zero, \Zero -> pat)
    LitPat _          -> (Zero, \Zero -> pat)
    NPat _ _ _        -> (Zero, \Zero -> pat)
    NPlusKPat _ _ _ _ -> (Zero, \Zero -> pat)
    TypePat _         -> (Zero, \Zero -> pat)
    SigPatIn (L s p) t -> (One p, \(One p') -> SigPatIn (L s p') t)
    SigPatOut (L s p) t -> (One p, \(One p') -> SigPatOut (L s p') t)
    CoPat w p t       -> (One p, \(One p') -> CoPat w p' t)

instance Biplate (HsConDetails (LPat id) (HsRecFields id (LPat id))) (Pat id) where
  biplate (PrefixCon ps) = down ps PrefixCon
  biplate (RecCon fs)    = down fs RecCon
  biplate (InfixCon (L sl l) (L sr r)) =
      (Two (One l) (One r),
      \(Two (One l') (One r')) -> InfixCon (L sl l') (L sr r'))

instance (Uniplate arg) => Biplate (HsRecFields id (Located arg)) arg where
  biplate (HsRecFields flds dotdot) =
      down flds (\flds' -> HsRecFields flds' dotdot)

instance (Uniplate arg) => Biplate (HsRecField id (Located arg)) arg where
  biplate (HsRecField lid (L sl arg) b) = 
      (One arg, \(One arg') -> HsRecField lid (L sl arg') b)

instance Biplate b a => Biplate (Bag b) a where
  biplate = uniplateOnBag biplate

--instance Biplate (HsBindLr n n) 

uniplateOnBag :: (a -> (U.Str b, U.Str b -> a))
              -> Bag a -> (U.Str b, U.Str b -> Bag a)
uniplateOnBag f bag = (as, \ns -> listToBag (bs ns))
                      where (as, bs) = uniplateOnList f (bagToList bag)


{-
deriving instance Show (Pat n)
deriving instance Show (Located l)
deriving instance Show (HsOverLit n)
deriving instance Show (HsExpr n)
deriving instance Show (HsQuasiQuote n)
deriving instance Show (HsSplice n)
deriving instance (Show idL, Show idR) => Show (HsLocalBindsLR idL idR)
deriving instance Show id => Show (HsGroup id)
deriving instance Show id => Show (HsBracket id)
deriving instance (Show id, Show arg) => Show (HsRecFields id arg)
deriving instance (Show id, Show arg) => Show (HsRecField id arg)
-}

down :: Biplate from to => from -> (from -> c) -> (U.Str to, U.Str to -> c)
down b f = (ps, \ps' -> f (set ps'))
  where (ps, set) = biplate b

-- BiplateType from to = (Str to, Str to -> from)

instance Biplate b a => Biplate (Located b) a where
  biplate (L s b) = down b (L s)
{-
instance Uniplate a => Biplate (Located a) a where
  biplate (L s a) = (One a, \(One a') -> L s a')
-}
instance Biplate b a => Biplate [b] a where
  biplate = uniplateOnList biplate
{-                           
fmap (fst . biplate) (listStr bs),
                strList . fmap (snd . biplate))
-}
{-                
  -- biplate :: (Str a, Str a -> [b])
  -- biplate :: (Str a, Str a -> b)
-}
{-
instance Biplate [(Located a)] a where
  biplate las = (unLoc `fmap` listStr las,
                 zipWith (\(L s _) a -> L s a) las . strList)
-}
{-
instance Biplate a b => Biplate (Bag a) b where
  biplate b = (foldBag 
-}
