{-# LANGUAGE PatternGuards, CPP #-}
module Scion.Inspect where

import GHC
import GHC.SYB.Instances

import Data.Map ( Map )
import qualified Data.Map as M

#ifdef DEBUG
import FastString
import Test.QuickCheck
import Test.GHC.Gen
#endif
------------------------------------------------------------------------------

typeDecls :: TypecheckedMod m => m -> [LTyClDecl Name]
typeDecls m | Just (grp, _, _, _, _) <- renamedSource m =
    [ t | t <- hs_tyclds grp
        , isDataDecl (unLoc t) 
            || isTypeDecl (unLoc t) 
            || isSynDecl (unLoc t) ]
    -- XXX: include families?
typeDecls _ = error "No renamer information available."

classDecls :: RenamedSource -> [LTyClDecl Name]
classDecls (grp, _, _, _, _) =
    [ t | t <- hs_tyclds grp
        , isClassDecl (unLoc t) ]

typeFamilyDecls :: RenamedSource -> [LTyClDecl Name]
typeFamilyDecls (grp, _, _, _, _) =
    [ t | t <- hs_tyclds grp
        , isFamilyDecl (unLoc t) ]

namesUsed :: TypecheckedModule -> [Name]
namesUsed tcm = []


data ThingAtPoint
  = ExprThing (HsExpr Name)
  | PatThing  (Pat Name)
  | NoThing

thingAtPoint :: TypecheckedMod m => m -> ThingAtPoint
thingAtPoint m = NoThing

------------------------------------------------------------------------------
data LocMap a
  = LocLeaf a  -- INVARIANT: not at top-level
  | LocNode (Map SrcSpan (LocMap a)) -- INVARIANT: non-overlapping, good SrcSpans

-- | Given two good SrcSpans (see 'SrcLoc.isGoodSrcSpan'), returns 'EQ' if the
-- spans overlap or, if not, the relative ordering of both.
cmpOverlap :: SrcSpan -> SrcSpan -> Ordering
cmpOverlap sp1 sp2
  | end1 < start2 = LT
  | end2 < start1 = GT
  | otherwise     = EQ
 where
   start1 = (srcSpanStartLine sp1, srcSpanStartCol sp1)
   end1   = (srcSpanEndLine sp1, srcSpanEndCol sp1)
   start2 = (srcSpanStartLine sp2, srcSpanStartCol sp2)
   end2   = (srcSpanEndLine sp2, srcSpanEndCol sp2)

#ifdef DEBUG

prop_invCmpOverlap s1 s2 =
  case cmpOverlap s1 s2 of
    LT -> cmpOverlap s2 s1 == GT
    EQ -> cmpOverlap s2 s1 == EQ
    GT -> cmpOverlap s2 s1 == LT

#endif
