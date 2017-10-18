module GHC.TypeCheck.Unify where

import GHC.Prelude
import GHC.TypeCheck.Util.CoreType ( TcTauType )
import GHC.TypeCheck.Util          ( TcM )
import GHC.TypeCheck.Evidence      ( TcCoercion )
import GHC.Syntax.Expression       ( HsExpr )
import GHC.Syntax.Type             ( HsType )
import GHC.Syntax.Extension        ( GhcRn )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: Maybe (HsExpr GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Maybe (HsType GhcRn) -> TcTauType -> TcTauType -> TcM TcCoercion
