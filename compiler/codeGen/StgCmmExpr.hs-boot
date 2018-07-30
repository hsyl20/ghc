module StgCmmExpr where

import StgCmmMonad( FCode, ReturnKind )
import StgSyn( StgExpr )

cgExpr :: StgExpr -> FCode ReturnKind
