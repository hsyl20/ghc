module GHC.CoreTypes.DataCon where

import GhcPrelude
import GHC.CoreTypes.Var( TyVar, TyVarBinder )
import GHC.CoreTypes.Name( Name, NamedThing )
import {-# SOURCE #-} GHC.CoreTypes.TyCon ( TyCon )
import GHC.CoreTypes.FieldLabel ( FieldLabel )
import GHC.CoreTypes.Unique ( Uniquable )
import GHC.Util.Outputable ( Outputable, OutputableBndr )
import GHC.CoreTypes.BasicTypes (Arity)
import {-# SOURCE #-} GHC.CoreTypes.Type.Internal ( Type, ThetaType )

data DataCon
data DataConRep
data EqSpec

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConExTyVars  :: DataCon -> [TyVar]
dataConUserTyVars :: DataCon -> [TyVar]
dataConUserTyVarBinders :: DataCon -> [TyVarBinder]
dataConSourceArity  :: DataCon -> Arity
dataConFieldLabels :: DataCon -> [FieldLabel]
dataConInstOrigArgTys  :: DataCon -> [Type] -> [Type]
dataConStupidTheta :: DataCon -> ThetaType
dataConFullSig :: DataCon
               -> ([TyVar], [TyVar], [EqSpec], ThetaType, [Type], Type)

instance Eq DataCon
instance Uniquable DataCon
instance NamedThing DataCon
instance Outputable DataCon
instance OutputableBndr DataCon
