module Type where

import Unbound.Generics.LocallyNameless (Alpha (..),Name)
import GHC.Entity.TypeConstructor

data TType = VarTy

type TyName = Name TType

instance Alpha TType where
  isTerm VarTy = False
