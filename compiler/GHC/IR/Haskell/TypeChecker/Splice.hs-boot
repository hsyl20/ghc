{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module GHC.IR.Haskell.TypeChecker.Splice where
import GHC.Entity.Name
import GHC.IR.Haskell.Syntax.Expression   ( PendingRnSplice )
import GHC.IR.Haskell.TypeChecker.Types( TcM, SpliceType )
import GHC.IR.Haskell.TypeChecker.Type   ( ExpRhoType )
import GHC.Entity.Annotation ( Annotation, CoreAnnTarget )
import GHC.IR.Haskell.Syntax.Extension ( GhcTcId, GhcRn, GhcPs )

import GHC.IR.Haskell.Syntax ( HsSplice, HsBracket, HsExpr, LHsExpr, LHsType,
                               LPat, LHsDecl, ThModFinalizers )
import qualified Language.Haskell.TH as TH

tcSpliceExpr :: HsSplice GhcRn
             -> ExpRhoType
             -> TcM (HsExpr GhcTcId)

tcUntypedBracket :: HsBracket GhcRn
                 -> [PendingRnSplice]
                 -> ExpRhoType
                 -> TcM (HsExpr GhcTcId)
tcTypedBracket :: HsBracket GhcRn
               -> ExpRhoType
               -> TcM (HsExpr GhcTcId)

runAnnotation     :: CoreAnnTarget -> LHsExpr GhcRn -> TcM Annotation

tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr GhcTcId) -> TcM (LHsExpr GhcTcId)

runMetaE :: LHsExpr GhcTcId -> TcM (LHsExpr GhcPs)
runMetaP :: LHsExpr GhcTcId -> TcM (LPat GhcPs)
runMetaT :: LHsExpr GhcTcId -> TcM (LHsType GhcPs)
runMetaD :: LHsExpr GhcTcId -> TcM [LHsDecl GhcPs]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a
runRemoteModFinalizers :: ThModFinalizers -> TcM ()
finishTH :: TcM ()
