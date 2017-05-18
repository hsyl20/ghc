module GHC.Compiler.CoreToInterface where

import {-# SOURCE #-} GHC.Types.TypeAndCoercion
import {-# SOURCE #-} GHC.Interface.Types ( IfaceType, IfaceTyCon
                                             , IfaceForAllBndr, IfaceCoercion
                                             , IfaceTyLit, IfaceTcArgs )
import GHC.Types.Var ( TyVarBinder )
import GHC.Types.TyCon ( TyCon )
import GHC.Types.Var.Set( VarSet )

-- For GHC.Types.TypeAndCoercion
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
toIfaceCoercion :: Coercion -> IfaceCoercion
