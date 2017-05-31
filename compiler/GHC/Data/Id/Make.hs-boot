module GHC.Data.Id.Make where
import GHC.Data.Name( Name )
import GHC.Data.Var( Id )
import Class( Class )
import {-# SOURCE #-} GHC.Data.DataCon( DataCon )
import {-# SOURCE #-} PrimOp( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
