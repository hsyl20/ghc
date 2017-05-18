module GHC.CoreTypes.PatternSynonym where

import GHC.CoreTypes.BasicTypes (Arity)
import {-# SOURCE #-} GHC.CoreTypes.Type.Internal (Type)
import GHC.CoreTypes.Var  (TyVar)
import GHC.CoreTypes.Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
