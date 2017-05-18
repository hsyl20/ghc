module GHC.IR.Haskell.TypeChecker.Unify where
import GHC.IR.Haskell.TypeChecker.Type     ( TcTauType )
import GHC.IR.Haskell.TypeChecker.Types    ( TcM )
import GHC.IR.Haskell.TypeChecker.Evidence ( TcCoercion )
import GHC.Utils.Outputable               ( Outputable )
import GHC.IR.Haskell.Syntax.Expression          ( HsExpr )
import GHC.IR.Haskell.Syntax.Extension    ( GhcRn )

-- This boot file exists only to tie the knot between
-- GHC.IR.Haskell.TypeChecker.Unify
-- and
-- GHC.IR.Haskell.TypeChecker.Instantiation

unifyType :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
noThing   :: Maybe (HsExpr GhcRn)
