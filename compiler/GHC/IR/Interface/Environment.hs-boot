module GHC.IR.Interface.Environment where

import GHC.Entity.Module
import GHC.Entity.OccName
import GHC.IR.Haskell.TypeChecker
import GHC.Entity.Name
import GHC.Entity.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
