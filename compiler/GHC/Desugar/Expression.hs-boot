module GHC.Desugar.Expression where
import GHC.Syntax    ( HsExpr, LHsExpr, LHsLocalBinds, SyntaxExpr )
import GHC.Data.Var      ( Id )
import GHC.Desugar.Monad  ( DsM )
import CoreSyn  ( CoreExpr )

dsExpr  :: HsExpr  Id -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr Id -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr Id -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds Id -> CoreExpr -> DsM CoreExpr
