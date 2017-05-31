{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsPat where
import GHC.Data.SrcLoc( Located )

import Data.Data hiding (Fixity)
import GHC.Utils.Outputable
import PlaceHolder      ( DataId, OutputableBndrId )

type role Pat nominal
data Pat (i :: *)
type LPat i = Located (Pat i)

instance (DataId id) => Data (Pat id)
instance (OutputableBndrId name) => Outputable (Pat name)
