module GHC.Packages.PackageConfig where

import GHC.Data.FastString
import {-# SOURCE #-} GHC.Data.Module
import GHC.PackageDb
newtype PackageName = PackageName FastString
newtype SourcePackageId = SourcePackageId FastString
type PackageConfig = InstalledPackageInfo ComponentId SourcePackageId PackageName UnitId ModuleName Module
