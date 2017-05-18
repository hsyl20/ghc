module GHC.Compiler.HaskellToCore.Match where
import GHC.Types.Var                   ( Id )
import GHC.Haskell.TypeCheck.Type  ( Type )
import GHC.Compiler.HaskellToCore.Monad ( DsM, EquationInfo, MatchResult )
import GHC.Core.Syntax               ( CoreExpr )
import GHC.Haskell.Syntax            ( LPat, HsMatchContext, MatchGroup
                                        , LHsExpr )
import GHC.Types.Name                  ( Name )
import GHC.Haskell.Syntax.Extension  ( GhcTc )

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
