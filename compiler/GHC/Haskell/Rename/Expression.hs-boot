module GHC.Haskell.Rename.Expression where
import GHC.CoreTypes.Name
import GHC.Haskell.Syntax
import GHC.CoreTypes.Name.Set             ( FreeVars )
import GHC.Haskell.TypeCheck.Util
import GHC.CoreTypes.SrcLoc               ( Located )
import GHC.Util.Outputable            ( Outputable )
import GHC.Haskell.Syntax.Extension ( GhcPs, GhcRn )

rnLExpr :: LHsExpr GhcPs
        -> RnM (LHsExpr GhcRn, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body GhcPs) => HsStmtContext Name
        -> (Located (body GhcPs) -> RnM (Located (body GhcRn), FreeVars))
        -> [LStmt GhcPs (Located (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GhcRn (Located (body GhcRn))], thing), FreeVars)
