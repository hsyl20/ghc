module ToIface where

import {-# SOURCE #-} TyCoRep
import {-# SOURCE #-} IfaceType
import GHC.Data.Var ( TyVar, TyVarBinder )
import GHC.Data.Type.Constructor ( TyCon )
import GHC.Data.Var.Set( VarSet )

-- For TyCoRep
toIfaceType :: Type -> IfaceType
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
toIfaceCoercion :: Coercion -> IfaceCoercion
