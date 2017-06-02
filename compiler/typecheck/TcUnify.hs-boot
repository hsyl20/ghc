module TcUnify where
import TcType     ( TcTauType )
import TcRnTypes  ( TcM )
import TcEvidence ( TcCoercion )
import GHC.Utils.Outputable ( Outputable )
import GHC.IR.Haskell.Expression     ( HsExpr )
import GHC.Data.Name       ( Name )

-- This boot file exists only to tie the knot between
--              TcUnify and Inst

unifyType :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
unifyKind :: Outputable a => Maybe a -> TcTauType -> TcTauType -> TcM TcCoercion
noThing   :: Maybe (HsExpr Name)
