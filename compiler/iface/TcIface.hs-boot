module TcIface where

import IfaceSyn    ( IfaceDecl, IfaceClsInst, IfaceFamInst, IfaceRule, IfaceAnnotation, IfaceType )
import TyCoRep     ( TyThing, Type )
import TcRnTypes   ( IfL )
import InstEnv     ( ClsInst )
import FamInstEnv  ( FamInst )
import CoreSyn     ( CoreRule )
import HscTypes    ( TypeEnv, VectInfo, IfaceVectInfo )
import Module      ( Module )
import Annotations ( Annotation )

tcIfaceType        :: IfaceType -> IfL Type
tcIfaceDecl        :: Bool -> IfaceDecl -> IfL TyThing
tcIfaceRules       :: Bool -> [IfaceRule] -> IfL [CoreRule]
tcIfaceVectInfo    :: Module -> TypeEnv -> IfaceVectInfo -> IfL VectInfo
tcIfaceInst        :: IfaceClsInst -> IfL ClsInst
tcIfaceFamInst     :: IfaceFamInst -> IfL FamInst
tcIfaceAnnotations :: [IfaceAnnotation] -> IfL [Annotation]
