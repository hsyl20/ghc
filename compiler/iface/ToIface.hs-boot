module ToIface where

import {-# SOURCE #-} GHC.Data.Types
import {-# SOURCE #-} IfaceType
import GHC.Data.Var ( TyVar, TyVarBinder )
import GHC.Data.Type.Constructor ( TyCon )
import GHC.Data.Var.Set( VarSet )

-- For GHC.Data.Types
toIfaceType :: Type -> IfaceType
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTvBndr :: TyVar -> IfaceTvBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
toIfaceCoercion :: Coercion -> IfaceCoercion
