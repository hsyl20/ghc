module GHC.Haskell.TypeCheck.Matches where
import GHC.Haskell.Syntax              ( GRHSs, MatchGroup, LHsExpr )
import GHC.Haskell.TypeCheck.Evidence( HsWrapper )
import GHC.Types.Name                    ( Name )
import GHC.Haskell.TypeCheck.Type    ( ExpRhoType, TcRhoType )
import GHC.Haskell.TypeCheck.Types   ( TcM )
import GHC.Types.SrcLoc                  ( Located )
import GHC.Haskell.Syntax.Extension    ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
