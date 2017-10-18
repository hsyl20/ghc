module GHC.TypeCheck.Module where

import GHC.Prelude
import GHC.Config.Flags             (DynFlags)
import GHC.CoreTypes.Type           (TyThing)
import GHC.TypeCheck.Util   (TcM)
import GHC.Util.Outputable          (SDoc)
import GHC.CoreTypes.Name           (Name)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> SDoc
badReexportedBootThing :: DynFlags -> Bool -> Name -> Name -> SDoc
