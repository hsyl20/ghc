module GHC.Interface.TypeCheck where

import GHC.Prelude
import GHC.Interface.Syntax         ( IfaceDecl, IfaceClsInst, IfaceFamInst
                                    , IfaceRule, IfaceAnnotation
                                    , IfaceCompleteMatch )
import GHC.CoreTypes.Type.Internal  ( TyThing )
import GHC.TypeCheck.Util   ( IfL )
import GHC.CoreTypes.Instance       ( ClsInst )
import GHC.CoreTypes.FamilyInstance ( FamInst )
import GHC.Core.Syntax              ( CoreRule )
import GHC.CoreTypes.Base           ( TypeEnv, VectInfo, IfaceVectInfo
                                    , CompleteMatch )
import GHC.CoreTypes.Module         ( Module )
import GHC.CoreTypes.Annotation     ( Annotation )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo     :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
