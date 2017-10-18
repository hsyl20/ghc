module GHC.HaskellToCore.Match where

import GHC.Prelude
import GHC.CoreTypes.Var                   ( Id )
import GHC.TypeCheck.Util.CoreType  ( Type )
import GHC.HaskellToCore.Monad ( DsM, EquationInfo, MatchResult )
import GHC.Core.Syntax               ( CoreExpr )
import GHC.Syntax            ( LPat, HsMatchContext, MatchGroup
                                        , LHsExpr )
import GHC.CoreTypes.Name                  ( Name )
import GHC.Syntax.Extension  ( GhcTc )

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
