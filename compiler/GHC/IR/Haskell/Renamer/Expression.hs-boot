module GHC.IR.Haskell.Renamer.Expression where
import GHC.Entity.Name
import GHC.IR.Haskell.Syntax
import GHC.Entity.Name.Set    ( FreeVars )
import GHC.IR.Haskell.TypeChecker.Types
import GHC.Entity.SrcLoc     ( Located )
import GHC.Utils.Outputable ( Outputable )
import GHC.IR.Haskell.Syntax.Extension ( GhcPs, GhcRn )

rnLExpr :: LHsExpr GhcPs
        -> RnM (LHsExpr GhcRn, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body GhcPs) => HsStmtContext Name
        -> (Located (body GhcPs) -> RnM (Located (body GhcRn), FreeVars))
        -> [LStmt GhcPs (Located (body GhcPs))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt GhcRn (Located (body GhcRn))], thing), FreeVars)
