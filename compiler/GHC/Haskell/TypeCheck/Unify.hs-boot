module GHC.Haskell.TypeCheck.Unify where
import GHC.Haskell.TypeCheck.Type     ( TcTauType )
import GHC.Haskell.TypeCheck.Types    ( TcM )
import GHC.Haskell.TypeCheck.Evidence ( TcCoercion )
import GHC.Utils.Outputable               ( Outputable )
import GHC.Haskell.Syntax.Expression          ( HsExpr )
import GHC.Haskell.Syntax.Extension    ( GhcRn )

-- This boot file exists only to tie the knot between
-- GHC.Haskell.TypeCheck.Unify
-- and
-- GHC.Haskell.TypeCheck.Instantiation

unifyType :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
noThing   :: Maybe (HsExpr GhcRn)
