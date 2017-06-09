module GHC.IR.Haskell.TypeSystem.Expression where
import GHC.IR.Haskell.Syntax    ( HsExpr, LHsExpr, SyntaxExpr )
import GHC.Data.Name     ( Name )
import import GHC.IR.Haskell.TypeSystem.Type   ( TcRhoType, TcSigmaType, SyntaxOpType, ExpType, ExpRhoType )
import TcRnTypes( TcM, TcId, CtOrigin )

tcPolyExpr ::
          LHsExpr Name
       -> TcSigmaType
       -> TcM (LHsExpr TcId)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr Name
       -> ExpRhoType
       -> TcM (LHsExpr TcId)

tcInferSigma, tcInferSigmaNC ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcSigmaType)

tcInferRho ::
          LHsExpr Name
       -> TcM (LHsExpr TcId, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> SyntaxExpr Name
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpType                  -- ^ overall result type
           -> ([TcSigmaType] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExpr TcId)

tcSyntaxOpGen :: CtOrigin
              -> SyntaxExpr Name
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> TcM a)
              -> TcM (a, SyntaxExpr TcId)


tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr TcId)
