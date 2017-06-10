module GHC.IR.Haskell.TypeSystem.Module where

import GHC.Config.Flags (DynFlags)
import GHC.Data.Type (TyThing)
import GHC.IR.Haskell.TypeSystem.Types (TcM)
import GHC.Utils.Outputable (SDoc)
import GHC.Data.Name (Name)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> SDoc
badReexportedBootThing :: DynFlags -> Bool -> Name -> Name -> SDoc