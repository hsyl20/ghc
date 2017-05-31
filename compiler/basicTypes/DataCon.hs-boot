module DataCon where
import GHC.Data.Var( TyVar, TyVarBinder )
import GHC.Data.Name( Name, NamedThing )
import {-# SOURCE #-} TyCon( TyCon )
import GHC.Data.FieldLabel ( FieldLabel )
import GHC.Data.Unique ( Uniquable )
import GHC.Utils.Outputable ( Outputable, OutputableBndr )
import BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep ( Type, ThetaType )

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
