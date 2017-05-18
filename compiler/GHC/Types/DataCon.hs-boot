module GHC.Types.DataCon where
import GHC.Types.Var( TyVar, TyVarBinder )
import GHC.Types.Name( Name, NamedThing )
import {-# SOURCE #-} GHC.Types.TyCon ( TyCon )
import GHC.Types.FieldLabel ( FieldLabel )
import GHC.Types.Unique ( Uniquable )
import GHC.Utils.Outputable ( Outputable, OutputableBndr )
import GHC.Types.BasicTypes (Arity)
import {-# SOURCE #-} GHC.Types.TypeAndCoercion ( Type, ThetaType )

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
