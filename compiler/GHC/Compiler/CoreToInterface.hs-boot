module GHC.Compiler.CoreToInterface where

import {-# SOURCE #-} GHC.Entity.TypeAndCoercion
import {-# SOURCE #-} GHC.IR.Interface.Types ( IfaceType, IfaceTyCon
                                             , IfaceForAllBndr, IfaceCoercion
                                             , IfaceTyLit, IfaceTcArgs )
import GHC.Entity.Var ( TyVarBinder )
import GHC.Entity.TypeConstructor ( TyCon )
import GHC.Entity.Var.Set( VarSet )

-- For GHC.Entity.TypeAndCoercion
toIfaceTypeX :: VarSet -> Type -> IfaceType
toIfaceTyLit :: TyLit -> IfaceTyLit
toIfaceForAllBndr :: TyVarBinder -> IfaceForAllBndr
toIfaceTyCon :: TyCon -> IfaceTyCon
toIfaceTcArgs :: TyCon -> [Type] -> IfaceTcArgs
toIfaceCoercion :: Coercion -> IfaceCoercion
