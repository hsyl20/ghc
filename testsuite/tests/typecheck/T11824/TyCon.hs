module TyCon where

import Unbound.Generics.LocallyNameless (Alpha (..))
import {-# SOURCE #-} GHC.Data.Type   (TyName)

data AlgTyConRhs
  = NewTyCon TyName

instance Alpha AlgTyConRhs where
  isTerm (NewTyCon nm) = isTerm nm
