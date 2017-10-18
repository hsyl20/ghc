module GHC.TypeCheck.Util.CoreType where
import GHC.Util.Outputable( SDoc )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTv :: TcTyVarDetails
