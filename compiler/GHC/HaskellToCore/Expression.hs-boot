module GHC.HaskellToCore.Expression where
import GHC.Haskell.Syntax             ( HsExpr, LHsExpr, LHsLocalBinds
                                         , SyntaxExpr )
import GHC.HaskellToCore.Monad  ( DsM )
import GHC.Core.Syntax                ( CoreExpr )
import GHC.Haskell.Syntax.Extension   ( GhcTc )

dsExpr  :: HsExpr GhcTc -> DsM CoreExpr
dsLExpr, dsLExprNoLP :: LHsExpr GhcTc -> DsM CoreExpr
dsSyntaxExpr :: SyntaxExpr GhcTc -> [CoreExpr] -> DsM CoreExpr
dsLocalBinds :: LHsLocalBinds GhcTc -> CoreExpr -> DsM CoreExpr
