module GHC.Interface.Environment where

import GHC.CoreTypes.Module
import GHC.CoreTypes.OccName
import GHC.TypeCheck.Monad
import GHC.CoreTypes.Name
import GHC.CoreTypes.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
