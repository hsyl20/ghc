module GHC.Haskell.TypeCheck.Environment where

import GHC.Haskell.TypeCheck.Util ( TcM )
import GHC.CoreTypes.Var.Free     ( TidyEnv )

-- Annoyingly, there's a recursion between tcInitTidyEnv (which does zonking and
-- hence needs GHC.Haskell.TypeCheck.Util.Monadic) and addErrTc etc which live
-- in GHC.Haskell.TypeCheck.Monad.  Rats.
tcInitTidyEnv :: TcM TidyEnv

