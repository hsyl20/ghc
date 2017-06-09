module GHC.Interface.TypeCheck where

import GHC.Interface.Syntax    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule,
                     IfaceAnnotation, IfaceCompleteMatch )
import GHC.Data.Types     ( TyThing )
import TcRnTypes   ( IfL )
import GHC.Data.ClassInstance     ( ClsInst )
import GHC.Data.FamilyInstance  ( FamInst )
import GHC.IR.Core.Syntax     ( CoreRule )
import GHC.Types    ( TypeEnv, VectInfo, IfaceVectInfo, CompleteMatch )
import GHC.Data.Module      ( Module )
import GHC.Data.Annotation ( Annotation )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo     :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
