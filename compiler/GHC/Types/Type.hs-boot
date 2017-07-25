{-# LANGUAGE FlexibleContexts #-}

module GHC.Types.Type where
import GHC.Types.TyCon
import GHC.Types.Var ( TyVar )
import {-# SOURCE #-} GHC.Types.TypeAndCoercion( Type, Coercion, Kind )
import GHC.Utils

isPredTy     :: Type -> Bool
isCoercionTy :: Type -> Bool

mkAppTy :: Type -> Type -> Type
mkCastTy :: Type -> Coercion -> Type
piResultTy :: Type -> Type -> Type

typeKind :: Type -> Kind
eqType :: Type -> Type -> Bool

partitionInvisibles :: TyCon -> (a -> Type) -> [a] -> ([a], [a])

coreView :: Type -> Maybe Type
tcView :: Type -> Maybe Type

tyCoVarsOfTypesWellScoped :: [Type] -> [TyVar]
tyCoVarsOfTypeWellScoped :: Type -> [TyVar]
splitTyConApp_maybe :: HasDebugCallStack => Type -> Maybe (TyCon, [Type])
