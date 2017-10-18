module GHC.TypeCheck.Match where
import GHC.Syntax              ( GRHSs, MatchGroup, LHsExpr )
import GHC.TypeCheck.Evidence( HsWrapper )
import GHC.CoreTypes.Name                    ( Name )
import GHC.TypeCheck.Util.CoreType    ( ExpRhoType, TcRhoType )
import GHC.TypeCheck.Util   ( TcM )
import GHC.CoreTypes.SrcLoc                  ( Located )
import GHC.Syntax.Extension    ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
