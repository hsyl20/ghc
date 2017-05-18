module GHC.Types.PatternSynonym where

import GHC.Types.BasicTypes (Arity)
import {-# SOURCE #-} GHC.Types.TypeAndCoercion (Type)
import GHC.Types.Var  (TyVar)
import GHC.Types.Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
