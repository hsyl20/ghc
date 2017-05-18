module GHC.Interface.TypeCheck where

import GHC.Interface.Syntax ( IfaceDecl, IfaceClsInst, IfaceFamInst,
                                 IfaceRule, IfaceAnnotation,
                                 IfaceCompleteMatch )
import GHC.Types.TypeAndCoercion         ( TyThing )
import GHC.Haskell.TypeCheck.Types   ( IfL )
import GHC.Types.Instance           ( ClsInst )
import GHC.Types.FamilyInstance          ( FamInst )
import GHC.Core.Syntax                 ( CoreRule )
import GHC.Types.Base                   ( TypeEnv, VectInfo, IfaceVectInfo
                                          , CompleteMatch )
import GHC.Types.Module                  ( Module )
import GHC.Types.Annotation              ( Annotation )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo     :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
