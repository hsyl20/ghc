module TcMatches where
import GHC.Syntax    ( GRHSs, MatchGroup, LHsExpr )
import TcEvidence( HsWrapper )
import GHC.Data.Name     ( Name )
import TcType   ( ExpRhoType, TcRhoType )
import TcRnTypes( TcM, TcId )
import GHC.Data.SrcLoc   ( Located )

tcGRHSsPat    :: GRHSs Name (LHsExpr Name)
              -> TcRhoType
              -> TcM (GRHSs TcId (LHsExpr TcId))

tcMatchesFun :: Located Name
             -> MatchGroup Name (LHsExpr Name)
             -> ExpRhoType
             -> TcM (HsWrapper, MatchGroup TcId (LHsExpr TcId))
