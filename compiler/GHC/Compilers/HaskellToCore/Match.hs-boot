module GHC.Compiler.HaskellToCore.Match where
import GHC.Data.Var      ( Id )
import GHC.IR.Haskell.TypeSystem.Type   ( Type )
import GHC.Compiler.HaskellToCore.Monad  ( DsM, EquationInfo, MatchResult )
import GHC.IR.Core.Syntax  ( CoreExpr )
import GHC.IR.Haskell.Syntax ( LPat, HsMatchContext, MatchGroup, LHsExpr )
import GHC.Data.Name     ( Name )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM MatchResult

matchWrapper
        :: HsMatchContext Name
        -> Maybe (LHsExpr Id)
        -> MatchGroup Id (LHsExpr Id)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext Name
        -> LPat Id
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePat
        :: CoreExpr
        -> HsMatchContext Name
        -> LPat Id
        -> Type
        -> MatchResult
        -> DsM MatchResult
