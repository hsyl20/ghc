module TcIface where

import IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule,
                     IfaceAnnotation, IfaceCompleteMatch )
import TyCoRep     ( TyThing )
import TcRnTypes   ( IfL )
import InstEnv     ( ClsInst )
import FamInstEnv  ( FamInst )
import CoreSyn     ( CoreRule )
import GHC.Types    ( TypeEnv, VectInfo, IfaceVectInfo, CompleteMatch )
import Module      ( Module )
import GHC.Types.Annotations ( Annotation )

tcIfaceDecl         :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules        :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo     :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst         :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst      :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations  :: [IfaceAnnotation] -> IfL [Annotation]
tcIfaceCompleteSigs :: [IfaceCompleteMatch] -> IfL [CompleteMatch]
