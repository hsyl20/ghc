module GHC.Builtin.Uniques where

import GHC.Prelude
import GHC.CoreTypes.Unique
import GHC.CoreTypes.Name
import GHC.CoreTypes.BasicTypes

-- Needed by GHC.Builtin.Types
knownUniqueName :: Unique -> Maybe Name

mkSumTyConUnique :: Arity -> Unique
mkSumDataConUnique :: ConTagZ -> Arity -> Unique

mkCTupleTyConUnique :: Arity -> Unique
mkCTupleDataConUnique :: Arity -> Unique

mkTupleTyConUnique :: Boxity -> Arity -> Unique
mkTupleDataConUnique :: Boxity -> Arity -> Unique
