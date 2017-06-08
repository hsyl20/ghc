-- Exists to allow GHC.Data.Types to import pretty-printers
module GHC.Interface.Types where

import GHC.Data.Var (TyVarBndr, ArgFlag)
import GHC.Data.Type.Constructor (TyConBndrVis)
import GHC.Data.BasicTypes (TyPrec)
import GHC.Utils.Outputable (Outputable, SDoc)
import GHC.Data.FastString (FastString)

type IfLclName = FastString
type IfaceKind = IfaceType
type IfacePredType = IfaceType

data ShowForAllFlag
data IfaceType
data IfaceTyCon
data IfaceTyLit
data IfaceCoercion
data IfaceTcArgs
type IfaceTvBndr = (IfLclName, IfaceKind)
type IfaceTyConBinder = TyVarBndr IfaceTvBndr TyConBndrVis
type IfaceForAllBndr  = TyVarBndr IfaceTvBndr ArgFlag

instance Outputable IfaceType

pprIfaceType, pprParendIfaceType :: IfaceType -> SDoc
pprIfaceSigmaType :: ShowForAllFlag -> IfaceType -> SDoc
pprIfaceTyLit :: IfaceTyLit -> SDoc
pprIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceTvBndr :: Bool -> IfaceTvBndr -> SDoc
pprUserIfaceForAll :: [IfaceForAllBndr] -> SDoc
pprIfaceContext :: [IfacePredType] -> SDoc
pprIfaceContextArr :: [IfacePredType] -> SDoc
pprIfaceTypeApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> SDoc
pprIfaceCoTcApp :: TyPrec -> IfaceTyCon -> [IfaceCoercion] -> SDoc
pprTyTcApp :: TyPrec -> IfaceTyCon -> IfaceTcArgs -> SDoc
pprIfacePrefixApp :: TyPrec -> SDoc -> [SDoc] -> SDoc
