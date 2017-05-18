module GHC.Interface.Environment where

import GHC.Types.Module
import GHC.Types.OccName
import GHC.Haskell.TypeCheck
import GHC.Types.Name
import GHC.Types.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
