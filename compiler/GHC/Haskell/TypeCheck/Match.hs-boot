module GHC.Haskell.TypeCheck.Match where
import GHC.Haskell.Syntax              ( GRHSs, MatchGroup, LHsExpr )
import GHC.Haskell.TypeCheck.Evidence( HsWrapper )
import GHC.CoreTypes.Name                    ( Name )
import GHC.Haskell.TypeCheck.Util.CoreType    ( ExpRhoType, TcRhoType )
import GHC.Haskell.TypeCheck.Util   ( TcM )
import GHC.CoreTypes.SrcLoc                  ( Located )
import GHC.Haskell.Syntax.Extension    ( GhcRn, GhcTcId )

tcGRHSsPat    :: GRHSs GhcRn (LHsExpr GhcRn)
              -> TcRhoType
              -> TcM (GRHSs GhcTcId (LHsExpr GhcTcId))

tcMatchesFun :: Located Name
             -> MatchGroup GhcRn (LHsExpr GhcRn)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup GhcTcId (LHsExpr GhcTcId))
