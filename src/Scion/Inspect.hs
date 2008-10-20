{-# LANGUAGE PatternGuards #-}
module Scion.Inspect where

import GHC
import GHC.SYB.Instances
import SrcLoc

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
    