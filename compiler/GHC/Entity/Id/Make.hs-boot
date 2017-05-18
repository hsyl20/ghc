module GHC.Entity.Id.Make where
import GHC.Entity.Name( Name )
import GHC.Entity.Var( Id )
import GHC.Entity.Class( Class )
import {-# SOURCE #-} GHC.Entity.DataConstructor ( DataCon )
import {-# SOURCE #-} GHC.Builtin.Primitive.Operations( PrimOp )

data DataConBoxer

mkDataConWorkId :: Name -> DataCon -> Id
mkDictSelId     :: Name -> Class   -> Id

mkPrimOpId      :: PrimOp -> Id

magicDictId :: Id
