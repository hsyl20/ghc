module GHC.CoreToInterface where

import {-# SOURCE #-} GHC.CoreTypes.Type.Internal
import {-# SOURCE #-} GHC.Interface.Type
                               ( IfaceType, IfaceTyCon, IfaceForAllBndr
                               , IfaceCoercion, IfaceTyLit, IfaceTcArgs )
import GHC.CoreTypes.Var     ( TyVarBinder )
import GHC.CoreTypes.TyCon   ( TyCon )
import GHC.CoreTypes.Var.Set ( VarSet )

-- For TyCoRep
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
toIfaceCoercionX :: VarSet -> Coercion -> IfaceCoercion
