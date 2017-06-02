module GHC.Compilers.SyntaxToCore.Match where
import GHC.Data.Var      ( Id )
import TcType   ( Type )
import GHC.Compilers.SyntaxToCore.Monad  ( DsM, EquationInfo, MatchResult )
import GHC.Core.Syntax  ( CoreExpr )
import GHC.Syntax    ( LPat, HsMatchContext, MatchGroup, LHsExpr )
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
