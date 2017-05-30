module IfaceEnv where

import Module
import GHC.Data.OccName
import TcRnMonad
import GHC.Data.Name
import SrcLoc

newGlobalBinder :: Module -> OccName -> SrcSpan -> TcRnIf a b Name
