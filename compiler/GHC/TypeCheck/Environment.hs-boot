module GHC.TypeCheck.Environment where

import GHC.TypeCheck.Util    ( TcM )
import GHC.CoreTypes.Var.Environment ( TidyEnv )

-- Annoyingly, there's a recursion between tcInitTidyEnv (which does zonking and
-- hence needs GHC.TypeCheck.Util.Monadic) and addErrTc etc which live
-- in GHC.TypeCheck.Monad.  Rats.
tcInitTidyEnv :: TcM TidyEnv

