module GHC.IR.Haskell.TypeChecker.Matches where
import GHC.IR.Haskell.Syntax ( GRHSs, MatchGroup, LHsExpr )
import GHC.IR.Haskell.TypeChecker.Evidence( HsWrapper )
import GHC.Entity.Name                   ( Name )
import GHC.IR.Haskell.TypeChecker.Type  ( ExpRhoType, TcRhoType )
import GHC.IR.Haskell.TypeChecker.Types ( TcM )
import GHC.Entity.SrcLoc                 ( Located )
import GHC.IR.Haskell.Syntax.Extension ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
