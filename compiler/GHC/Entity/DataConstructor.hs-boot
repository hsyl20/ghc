module GHC.Entity.DataConstructor where
import GHC.Entity.Var( TyVar, TyVarBinder )
import GHC.Entity.Name( Name, NamedThing )
import {-# SOURCE #-} GHC.Entity.TypeConstructor ( TyCon )
import GHC.Entity.FieldLabel ( FieldLabel )
import GHC.Entity.Unique ( Uniquable )
import GHC.Utils.Outputable ( Outputable, OutputableBndr )
import GHC.Entity.BasicTypes (Arity)
import {-# SOURCE #-} GHC.Entity.TypeAndCoercion ( Type, ThetaType )

data DataCon
data DataConRep
data EqSpec
filterEqSpec :: [EqSpec] -> [TyVarBinder] -> [TyVarBinder]

dataConName      :: DataCon -> Name
dataConTyCon     :: DataCon -> TyCon
dataConUnivTyVarBinders :: DataCon -> [TyVarBinder]
dataConExTyVars  :: DataCon -> [TyVar]
dataConExTyVarBinders :: DataCon -> [TyVarBinder]
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
