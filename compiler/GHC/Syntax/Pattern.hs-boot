{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module GHC.Syntax.Pattern where
import GHC.CoreTypes.SrcLoc( Located )

import Data.Data hiding (Fixity)
import GHC.Util.Outputable
import GHC.Syntax.Extension ( SourceTextX, DataId, OutputableBndrId )

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (DataId p) => Data (Pat p)
instance (SourceTextX pass, OutputableBndrId pass) => Outputable (Pat pass)
