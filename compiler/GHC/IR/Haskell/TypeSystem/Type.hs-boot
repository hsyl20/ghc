module GHC.IR.Haskell.TypeSystem.Type where
import GHC.Utils.Outputable( SDoc )

data MetaDetails

data TcTyVarDetails
pprTcTyVarDetails :: TcTyVarDetails -> SDoc
vanillaSkolemTv :: TcTyVarDetails
