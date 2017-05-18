module GHC.IR.Haskell.TypeChecker.Module where

import GHC.Config.Flags (DynFlags)
import GHC.Entity.Type (TyThing)
import GHC.IR.Haskell.TypeChecker.Types (TcM)
import GHC.Utils.Outputable (SDoc)
import GHC.Entity.Name (Name)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> SDoc
badReexportedBootThing :: DynFlags -> Bool -> Name -> Name -> SDoc
