module GHC.IR.Interface.TypeChecker where

import GHC.IR.Interface.Syntax ( IfaceDecl, IfaceClsInst, IfaceFamInst,
                                 IfaceRule, IfaceAnnotation,
                                 IfaceCompleteMatch )
import GHC.Entity.TypeAndCoercion     ( TyThing )
import GHC.IR.Haskell.TypeChecker.Types   ( IfL )
import GHC.Entity.ClassInstance     ( ClsInst )
import GHC.Entity.FamilyInstance  ( FamInst )
import GHC.IR.Core.Syntax     ( CoreRule )
import GHC.Entity.Types    ( TypeEnv, VectInfo, IfaceVectInfo, CompleteMatch )
import GHC.Entity.Module      ( Module )
import GHC.Entity.Annotation ( Annotation )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo     :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
