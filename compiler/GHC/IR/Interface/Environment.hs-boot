module GHC.IR.Interface.Environment where

import GHC.Data.Module
import GHC.Data.OccName
import GHC.IR.Haskell.TypeSystem
import GHC.Data.Name
import GHC.Data.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
