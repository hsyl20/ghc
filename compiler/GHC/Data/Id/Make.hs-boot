module GHC.Data.Id.Make where
import GHC.Data.Name( Name )
import GHC.Data.Var( Id )
import GHC.Data.Class( Class )
import {-# SOURCE #-} GHC.Data.DataConstructor ( DataCon )
import {-# SOURCE #-} GHC.Builtin.Primitive.Operations( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
