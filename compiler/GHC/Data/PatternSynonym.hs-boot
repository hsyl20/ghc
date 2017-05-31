module GHC.Data.PatternSynonym where

import GHC.Data.BasicTypes (Arity)
import {-# SOURCE #-} TyCoRep (Type)
import GHC.Data.Var (TyVar)
import GHC.Data.Name (Name)

data PatSyn

patSynArity :: PatSyn -> Arity
patSynInstArgTys :: PatSyn -> [Type] -> [Type]
patSynExTyVars :: PatSyn -> [TyVar]
patSynName :: PatSyn -> Name
