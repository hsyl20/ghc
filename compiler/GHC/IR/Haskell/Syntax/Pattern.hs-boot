{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.IR.Haskell.Syntax.Pattern where
import GHC.Entity.SrcLoc( Located )

import Data.Data hiding (Fixity)
import GHC.Utils.Outputable
import GHC.IR.Haskell.Syntax.Extension      ( SourceTextX, DataId, OutputableBndrId )

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (DataId p) => Data (Pat p)
instance (SourceTextX pass, OutputableBndrId pass) => Outputable (Pat pass)
