module GHC.Compiler.HaskellToCore.Match where
import GHC.Entity.Var                   ( Id )
import GHC.IR.Haskell.TypeChecker.Type  ( Type )
import GHC.Compiler.HaskellToCore.Monad ( DsM, EquationInfo, MatchResult )
import GHC.IR.Core.Syntax               ( CoreExpr )
import GHC.IR.Haskell.Syntax            ( LPat, HsMatchContext, MatchGroup
                                        , LHsExpr )
import GHC.Entity.Name                  ( Name )
import GHC.IR.Haskell.Syntax.Extension  ( GhcTc )

match   :: [Id]
        -> Type
        -> [EquationInfo]
        -> DsM MatchResult

matchWrapper
        :: HsMatchContext Name
        -> Maybe (LHsExpr GhcTc)
        -> MatchGroup GhcTc (LHsExpr GhcTc)
        -> DsM ([Id], CoreExpr)

matchSimply
        :: CoreExpr
        -> HsMatchContext Name
        -> LPat GhcTc
        -> CoreExpr
        -> CoreExpr
        -> DsM CoreExpr

matchSinglePat
        :: CoreExpr
        -> HsMatchContext Name
        -> LPat GhcTc
        -> Type
        -> MatchResult
        -> DsM MatchResult
