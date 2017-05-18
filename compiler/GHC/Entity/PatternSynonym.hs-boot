module GHC.Entity.PatternSynonym where

import GHC.Entity.BasicTypes (Arity)
import {-# SOURCE #-} GHC.Entity.TypeAndCoercion (Type)
import GHC.Entity.Var (TyVar)
import GHC.Entity.Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
