module GHC.Compiler.HaskellToCore.Expression where
import GHC.IR.Haskell.Syntax             ( HsExpr, LHsExpr, LHsLocalBinds
                                         , SyntaxExpr )
import GHC.Compiler.HaskellToCore.Monad  ( DsM )
import GHC.IR.Core.Syntax                ( CoreExpr )
import GHC.IR.Haskell.Syntax.Extension   ( GhcTc )

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
