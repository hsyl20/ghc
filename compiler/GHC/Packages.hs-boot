module GHC.Packages where

import GHC.Prelude
import {-# SOURCE #-} GHC.Config.Flags (DynFlags)
import {-# SOURCE #-} GHC.CoreTypes.Module (ComponentId, UnitId, InstalledUnitId)
data PackageState
data PackageConfigMap
emptyPackageState :: PackageState
componentIdString :: DynFlags -> ComponentId -> Maybe String
displayInstalledUnitId :: DynFlags -> InstalledUnitId -> Maybe String
improveUnitId :: PackageConfigMap -> UnitId -> UnitId
getPackageConfigMap :: DynFlags -> PackageConfigMap
