module IfaceEnv where

import GHC.Data.Module
import GHC.Data.OccName
import TcRnMonad
import GHC.Data.Name
import GHC.Data.SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
