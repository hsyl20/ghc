{-# LANGUAGE FlexibleContexts #-}

module GHC.CoreTypes.Type where

import GhcPrelude
import GHC.CoreTypes.TyCon
import GHC.CoreTypes.Var ( TyCoVar )
import {-# SOURCE #-} GHC.CoreTypes.Type.Internal( Type, Coercion )
import GHC.Util

isPredTy     :: Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy :: Type -> Type -> Type
mkCastTy :: Type -> Coercion -> Type
piResultTy :: HasDebugCallStack => Type -> Type -> Type

eqType :: Type -> Type -> Bool

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type

tyCoVarsOfTypesWellScoped :: [Type] -> [TyCoVar]
tyCoVarsOfTypeWellScoped :: Type -> [TyCoVar]
toposortTyVars :: [TyCoVar] -> [TyCoVar]
splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
