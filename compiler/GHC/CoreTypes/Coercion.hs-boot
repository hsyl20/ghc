{-# LANGUAGE FlexibleContexts #-}

module GHC.CoreTypes.Coercion where

import GHC.Prelude

import {-# SOURCE #-} GHC.CoreTypes.Type.Internal
import {-# SOURCE #-} GHC.CoreTypes.TyCon

import GHC.CoreTypes.BasicTypes ( LeftOrRight )
import GHC.CoreTypes.Coercion.Axiom
import GHC.CoreTypes.Var
import GHC.Data.Pair
import GHC.Util

mkReflCo :: Role -> Type -> Coercion
mkTyConAppCo :: HasDebugCallStack => Role -> TyCon -> [Coercion] -> Coercion
mkAppCo :: Coercion -> Coercion -> Coercion
mkForAllCo :: TyVar -> Coercion -> Coercion -> Coercion
mkFunCo :: Role -> Coercion -> Coercion -> Coercion
mkCoVarCo :: CoVar -> Coercion
mkAxiomInstCo :: CoAxiom Branched -> BranchIndex -> [Coercion] -> Coercion
mkPhantomCo :: Coercion -> Type -> Type -> Coercion
mkUnsafeCo :: Role -> Type -> Type -> Coercion
mkUnivCo :: UnivCoProvenance -> Role -> Type -> Type -> Coercion
mkSymCo :: Coercion -> Coercion
mkTransCo :: Coercion -> Coercion -> Coercion
mkNthCo :: Int -> Coercion -> Coercion
mkLRCo :: LeftOrRight -> Coercion -> Coercion
mkInstCo :: Coercion -> Coercion -> Coercion
mkCoherenceCo :: Coercion -> Coercion -> Coercion
mkKindCo :: Coercion -> Coercion
mkSubCo :: Coercion -> Coercion
mkProofIrrelCo :: Role -> Coercion -> Coercion -> Coercion -> Coercion

mkFunCos :: Role -> [Coercion] -> Coercion -> Coercion

isReflCo :: Coercion -> Bool
isReflexiveCo :: Coercion -> Bool
coVarKindsTypesRole :: CoVar -> (Kind, Kind, Type, Type, Role)
coVarRole :: CoVar -> Role

mkCoercionType :: Role -> Type -> Type -> Type

data LiftingContext
liftCoSubst :: HasDebugCallStack => Role -> LiftingContext -> Type -> Coercion
seqCo :: Coercion -> ()

coercionKind :: Coercion -> Pair Type
coercionType :: Coercion -> Type
