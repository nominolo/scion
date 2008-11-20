{-# LANGUAGE FlexibleInstances, CPP #-}
-- |
-- Module      : Scion.Inspect.Search
-- Copyright   : (c) Thomas Schilling 2008
-- License     : BSD-style
--
-- Maintainer  : nominolo@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- Find things in a syntax tree.
--
module Scion.Inspect.Find 
  ( findHsThing, SearchResult(..), SearchResults
  , surrounds, overlaps
#ifdef DEBUG
  , prop_invCmpOverlap
#endif
  ) 
where

import Scion.Utils()

import GHC
import Bag
import Outputable

import Data.Tree
import Data.Foldable as F ( toList )

------------------------------------------------------------------------------

-- | Lookup all the things in a certain region.
findHsThing :: Search a => (SrcSpan -> Bool) -> a -> SearchResults
findHsThing p a = search p noSrcSpan a

data SearchResult
  = FoundBind SrcSpan (HsBind Name)
  | FoundPat  SrcSpan (Pat Name)
  | FoundType SrcSpan (HsType Name)
  | FoundExpr SrcSpan (HsExpr Name)
  | FoundStmt SrcSpan (Stmt Name)

type SearchResults = Forest SearchResult


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
   -- TODO: don't ignore file name

surrounds :: SrcSpan -> SrcSpan -> Bool
surrounds outer inner = start1 <= start2 && end2 <= end1
  where
   start1 = (srcSpanStartLine outer, srcSpanStartCol outer)
   end1   = (srcSpanEndLine outer, srcSpanEndCol outer)
   start2 = (srcSpanStartLine inner, srcSpanStartCol inner)
   end2   = (srcSpanEndLine inner, srcSpanEndCol inner)

overlaps :: SrcSpan -> SrcSpan -> Bool
overlaps sp1 sp2 = cmpOverlap sp1 sp2 == EQ
    

#ifdef DEBUG

prop_invCmpOverlap :: SrcSpan -> SrcSpan -> Bool
prop_invCmpOverlap s1 s2 =
  case cmpOverlap s1 s2 of
    LT -> cmpOverlap s2 s1 == GT
    EQ -> cmpOverlap s2 s1 == EQ
    GT -> cmpOverlap s2 s1 == LT

-- prop_sane : if overlap -> there is some point which is in both spans

#endif


------------------------------------------------------------------------------

instance Outputable SearchResult where
  ppr (FoundBind s b) = text "bind:" <+> ppr s $$ nest 4 (ppr b)
  ppr (FoundPat s b)  = text "pat: " <+> ppr s $$ nest 4 (ppr b)
  ppr (FoundType s t) = text "type:" <+> ppr s $$ nest 4 (ppr t)
  ppr (FoundExpr s e) = text "expr:" <+> ppr s $$ nest 4 (ppr e)
  ppr (FoundStmt s t) = text "stmt:" <+> ppr s $$ nest 4 (ppr t)

instance Outputable a => Outputable (Tree a) where
  ppr (Node v cs) = ppr v $$ nest 2 (vcat (map ppr cs))

class Search a where
  search :: (SrcSpan -> Bool) -> SrcSpan -> a -> SearchResults

only :: SearchResult -> SearchResults
only r = [Node r []]

above :: SearchResult -> SearchResults -> SearchResults
above r rest = [Node r rest]

instance Search a => Search (Located a) where
  search p _ (L s a)
    | p s   = search p s a
    | otherwise = []

instance Search a => Search (Bag a) where
  search p s bs = concatMap (search p s) (F.toList bs)

instance Search a => Search [a] where
  search p s bs = concatMap (search p s) bs

instance Search a => Search (Maybe a) where
  search _ _ Nothing = []
  search p s (Just a) = search p s a

instance Search (HsBindLR Name Name) where
  search p s b = FoundBind s b `above` search_inside
    where
      search_inside = 
        case b of
          FunBind { fun_matches = ms } -> search p s ms
          _ -> []

instance Search (MatchGroup Name) where
  search p s (MatchGroup ms _) = search p s ms

instance Search (Match Name) where
  search p s (Match pats tysig rhss) =
    search p s pats ++ search p s tysig ++ search p s rhss
    
instance Search (Pat Name) where
  search _ s p = only (FoundPat s p)

instance Search (HsType Name) where
  search _ s t = only (FoundType s t)

instance Search (GRHSs Name) where
  search p s (GRHSs rhss local_binds) =
    search p s rhss ++ search p s local_binds

instance Search (GRHS Name) where
  search p s (GRHS _guards rhs) =
    -- guards look like statements, but we should probably treat them
    -- differently
    search p s rhs

instance Search (HsExpr Name) where
  search p s e0 = FoundExpr s e0 `above` search_inside
    where
      search_inside = 
        case e0 of
          HsLam mg -> search p s mg
          HsApp l r -> search p s l ++ search p s r
          OpApp l o _ r -> search p s l ++ search p s o ++ search p s r
          NegApp e n    -> search p s e ++ search p s n
          HsPar e       -> search p s e
          SectionL e o  -> search p s e ++ search p s o
          SectionR o e  -> search p s o ++ search p s e
          HsCase e mg   -> search p s e ++ search p s mg
          HsIf c t e    -> search p s c ++ search p s t ++ search p s e
          HsLet bs e    -> search p s bs ++ search p s e
          HsDo _ ss e _ -> search p s ss ++ search p s e
          ExplicitList _ es     -> search p s es
          ExplicitPArr _ es     -> search p s es
          ExplicitTuple es _    -> search p s es
          RecordCon _ _ bs      -> search p s bs
          RecordUpd es bs _ _ _ -> search p s es ++ search p s bs
          ExprWithTySig e t     -> search p s e ++ search p s t
          ExprWithTySigOut e t  -> search p s e ++ search p s t
          ArithSeq _ i          -> search p s i
          PArrSeq _ i           -> search p s i
          HsSCC _ e             -> search p s e
          HsCoreAnn _ e         -> search p s e
          -- TODO: template haskell stuff
          -- TODO: arrow stuff
          HsTick _ _ e     -> search p s e
          HsBinTick _ _ e  -> search p s e
          HsTickPragma _ e -> search p s e 
          HsWrap _ e       -> search p s e
          _ -> []

instance Search (HsLocalBindsLR Name Name) where
  search p s (HsValBinds val_binds) = search p s val_binds
  search _ _ _ = []

instance Search (HsValBindsLR Name Name) where
  search p s (ValBindsOut rec_binds _) =
      concatMap (search p s . snd) rec_binds
  search _ _ _ = []

instance Search (StmtLR Name Name) where
  search p s st 
    | RecStmt _ _ _ _ _ <- st = search_inside -- see Note [SearchRecStmt]
    | otherwise               = FoundStmt s st `above` search_inside
    where
      search_inside =
        case st of
          BindStmt pat e _ _ -> search p s pat ++ search p s e
          ExprStmt e _ _     -> search p s e
          LetStmt bs         -> search p s bs
          ParStmt ss         -> search p s (concatMap fst ss)
          TransformStmt (ss,_) f e -> search p s ss ++ search p s f
                                      ++ search p s e
          GroupStmt (ss, _) g -> search p s ss ++ search p s g
          RecStmt ss _ _ _ _ -> search p s ss

--
-- Note [SearchRecStmt]
-- --------------------
--
-- We only return children of a RecStmt but not the RecStmt itself, even
-- though a RecStmt may occur in the source code (under very rare
-- circumstances).  The reasons are:
--
--  * We have no way of knowing whether the RecStmt actually occured in the
--    source code.  We could add a flag in GHC, but its probably not
--    worthwhile due to the other reason.
--
--  * GHC may move things out of the recursive group if it detects that these
--    things are in fact not recursive at all.  Source locations are
--    preserved, so this is fine.
--

instance Search (GroupByClause Name) where
  search p s (GroupByNothing f) = search p s f
  search p s (GroupBySomething using_f e) =
      either (search p s) (const []) using_f ++ search p s e

instance Search (ArithSeqInfo Name) where
  search p s (From e)         = search p s e
  search p s (FromThen e1 e2) = search p s e1 ++ search p s e2
  search p s (FromTo e1 e2)   = search p s e1 ++ search p s e2
  search p s (FromThenTo e1 e2 e3) = 
      search p s e1 ++ search p s e2 ++ search p s e3

-- type HsRecordBinds id = HsRecFields id (LHsExpr id)
instance Search e => Search (HsRecFields Name e) where
  search p s (HsRecFields flds _) = search p s flds

instance Search e => Search (HsRecField Name e) where
  search p s (HsRecField _lid a _) = search p s a

