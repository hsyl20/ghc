module GHC.Haskell.TypeCheck.Unify where

import GHC.Prelude
import GHC.Haskell.TypeCheck.Util.CoreType ( TcTauType )
import GHC.Haskell.TypeCheck.Util          ( TcM )
import GHC.Haskell.TypeCheck.Evidence      ( TcCoercion )
import GHC.Haskell.Syntax.Expression       ( HsExpr )
import GHC.Haskell.Syntax.Type             ( HsType )
import GHC.Haskell.Syntax.Extension        ( GhcRn )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: Maybe (HsExpr GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Maybe (HsType GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
