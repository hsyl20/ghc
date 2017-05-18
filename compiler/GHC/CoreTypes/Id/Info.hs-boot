module GHC.CoreTypes.Id.Info where

import GhcPrelude
import GHC.Util.Outputable
data IdInfo
data IdDetails

vanillaIdInfo :: IdInfo
coVarDetails :: IdDetails
isCoVarDetails :: IdDetails -> Bool
pprIdDetails :: IdDetails -> SDoc

