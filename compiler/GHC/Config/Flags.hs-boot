
module GHC.Config.Flags where

import Platform

data DynFlags
data DumpFlag

targetPlatform       :: DynFlags -> Platform
pprUserLength        :: DynFlags -> Int
pprCols              :: DynFlags -> Int
unsafeGlobalDynFlags :: DynFlags
useUnicode           :: DynFlags -> Bool
useUnicodeSyntax     :: DynFlags -> Bool
shouldUseColor       :: DynFlags -> Bool
hasPprDebug          :: DynFlags -> Bool
hasNoDebugOutput     :: DynFlags -> Bool
