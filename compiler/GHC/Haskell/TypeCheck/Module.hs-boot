module GHC.Haskell.TypeCheck.Module where

import GHC.Config.Flags                   (DynFlags)
import GHC.Types.Type                    (TyThing)
import GHC.Haskell.TypeCheck.Types   (TcM)
import GHC.Utils.Outputable               (SDoc)
import GHC.Types.Name                    (Name)

checkBootDeclM :: Bool  -- ^ True <=> an hs-boot file (could also be a sig)
               -> TyThing -> TyThing -> TcM ()
missingBootThing :: Bool -> Name -> String -> SDoc
badReexportedBootThing :: DynFlags -> Bool -> Name -> Name -> SDoc
