module GHC.Types.Id.Make where
import GHC.Types.Name( Name )
import GHC.Types.Var( Id )
import GHC.Types.Class( Class )
import {-# SOURCE #-} GHC.Types.DataCon ( DataCon )
import {-# SOURCE #-} GHC.Builtin.Primitive.Operations( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
