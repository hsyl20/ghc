module GHC.Haskell.TypeCheck.Expression where
import GHC.CoreTypes.Name
import GHC.Haskell.Syntax           ( HsExpr, LHsExpr, SyntaxExpr )
import GHC.Haskell.TypeCheck.Util.CoreType ( TcRhoType, TcSigmaType, SyntaxOpType,
                                         ExpType, ExpRhoType )
import GHC.Haskell.TypeCheck.Util( TcM, CtOrigin )
import GHC.Haskell.Syntax.Extension ( GhcRn, GhcTcId )

tcPolyExpr ::
          LHsExpr GhcRn
       -> TcSigmaType
       -> TcM (LHsExpr GhcTcId)

tcMonoExpr, tcMonoExprNC ::
          LHsExpr GhcRn
       -> ExpRhoType
       -> TcM (LHsExpr GhcTcId)

tcInferSigma, tcInferSigmaNC ::
          LHsExpr GhcRn
       -> TcM (LHsExpr GhcTcId, TcSigmaType)

tcInferRho ::
          LHsExpr GhcRn
       -> TcM (LHsExpr GhcTcId, TcRhoType)

tcSyntaxOp :: CtOrigin
           -> SyntaxExpr GhcRn
           -> [SyntaxOpType]           -- ^ shape of syntax operator arguments
           -> ExpType                  -- ^ overall result type
           -> ([TcSigmaType] -> TcM a) -- ^ Type check any arguments
           -> TcM (a, SyntaxExpr GhcTcId)

tcSyntaxOpGen :: CtOrigin
              -> SyntaxExpr GhcRn
              -> [SyntaxOpType]
              -> SyntaxOpType
              -> ([TcSigmaType] -> TcM a)
              -> TcM (a, SyntaxExpr GhcTcId)


tcCheckId :: Name -> ExpRhoType -> TcM (HsExpr GhcTcId)
