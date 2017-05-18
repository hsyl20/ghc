-- Used only by GHC/Compiler/CoreToInterface.hs-boot

module GHC.IR.Interface.Types( IfaceType, IfaceTyCon, IfaceForAllBndr
                             , IfaceCoercion, IfaceTyLit, IfaceTcArgs ) where

import GHC.Entity.Var (TyVarBndr, ArgFlag)
import GHC.Data.FastString (FastString)

data IfaceTcArgs
type IfLclName = FastString
type IfaceKind = IfaceType

data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
type IfaceTvBndr      = (IfLclName, IfaceKind)
type IfaceForAllBndr  = TyVarBndr IfaceTvBndr ArgFlag
