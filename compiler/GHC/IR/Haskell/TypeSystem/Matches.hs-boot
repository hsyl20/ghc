module GHC.IR.Haskell.TypeSystem.Matches where
import GHC.IR.Haskell.Syntax ( GRHSs, MatchGroup, LHsExpr )
import GHC.IR.Haskell.TypeSystem.Evidence( HsWrapper )
import GHC.Data.Name     ( Name )
import import GHC.IR.Haskell.TypeSystem.Type   ( ExpRhoType, TcRhoType )
import TcRnTypes( TcM, TcId )
import GHC.Data.SrcLoc   ( Located )

tcGRHSsPat    :: GRHSs Name (LHsExpr Name)
              -> TcRhoType
              -> TcM (GRHSs TcId (LHsExpr TcId))

tcMatchesFun :: Located Name
             -> MatchGroup Name (LHsExpr Name)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId))
