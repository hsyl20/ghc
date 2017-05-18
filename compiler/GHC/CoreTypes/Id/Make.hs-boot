module GHC.CoreTypes.Id.Make where
import GHC.CoreTypes.Name( Name )
import GHC.CoreTypes.Var( Id )
import GHC.CoreTypes.Class( Class )
import {-# SOURCE #-} GHC.CoreTypes.DataCon ( DataCon )
import {-# SOURCE #-} GHC.Builtin.Primitive.Operations( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
