module GHC.IR.Haskell.Renamer.Expression where
import GHC.IR.Haskell.Syntax
import GHC.Data.Name       ( Name )
import GHC.Data.Name.Set    ( FreeVars )
import GHC.Data.RdrName    ( RdrName )
import TcRnTypes
import GHC.Data.SrcLoc     ( Located )
import GHC.Utils.Outputable ( Outputable )

rnLExpr :: LHsExpr RdrName
        -> RnM (LHsExpr Name, FreeVars)

rnStmts :: --forall thing body.
           Outputable (body RdrName) => HsStmtContext Name
        -> (Located (body RdrName) -> RnM (Located (body Name), FreeVars))
        -> [LStmt RdrName (Located (body RdrName))]
        -> ([Name] -> RnM (thing, FreeVars))
        -> RnM (([LStmt Name (Located (body Name))], thing), FreeVars)
