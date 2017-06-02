module GHC.Compilers.SyntaxToCore.Expression where
import GHC.Syntax    ( HsExpr, LHsExpr, LHsLocalBinds, SyntaxExpr )
import GHC.Data.Var      ( Id )
import GHC.Compilers.SyntaxToCore.Monad  ( DsM )
import GHC.IR.Core.Syntax  ( CoreExpr )

dsExpr  :: HsExpr  Id -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr Id -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr Id -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds Id -> CoreExpr -> DsM CoreExpr
