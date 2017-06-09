module GHC.IR.Haskell.TypeSystem.Unify where
import import GHC.IR.Haskell.TypeSystem.Type     ( TcTauType )
import TcRnTypes  ( TcM )
import TcEvidence ( TcCoercion )
import GHC.Utils.Outputable ( Outputable )
import GHC.IR.Haskell.Expression     ( HsExpr )
import GHC.Data.Name       ( Name )

-- This boot file exists only to tie the knot between
--              GHC.IR.Haskell.TypeSystem.Unify and Inst

unifyType :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
noThing   :: Maybe (HsExpr Name)
