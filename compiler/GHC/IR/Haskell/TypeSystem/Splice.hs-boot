{-# LANGUAGE CPP #-}

module GHC.IR.Haskell.TypeSystem.Splice where
import GHC.IR.Haskell.Syntax    ( HsSplice, HsBracket, HsExpr, LHsExpr )
import GHC.IR.Haskell.Expression   ( PendingRnSplice )
import GHC.Data.Name     ( Name )
import GHC.IR.Haskell.TypeSystem.Types( TcM, TcId )
import GHC.IR.Haskell.TypeSystem.Type   ( ExpRhoType )
import GHC.Data.Annotation ( Annotation, CoreAnnTarget )

import GHC.IR.Haskell.Syntax      ( LHsType, LPat, LHsDecl, ThModFinalizers )
import GHC.Data.RdrName    ( RdrName )
import GHC.IR.Haskell.TypeSystem.Types  ( SpliceType )
import qualified Language.Haskell.TH as TH

tcSpliceExpr :: HsSplice Name
             -> ExpRhoType
             -> TcM (HsExpr TcId)

tcUntypedBracket :: HsBracket Name
                 -> [PendingRnSplice]
                 -> ExpRhoType
                 -> TcM (HsExpr TcId)
tcTypedBracket :: HsBracket Name
               -> ExpRhoType
               -> TcM (HsExpr TcId)

runAnnotation     :: CoreAnnTarget -> LHsExpr Name -> TcM Annotation

tcTopSpliceExpr :: SpliceType -> TcM (LHsExpr TcId) -> TcM (LHsExpr TcId)

runMetaE :: LHsExpr TcId -> TcM (LHsExpr RdrName)
runMetaP :: LHsExpr TcId -> TcM (LPat RdrName)
runMetaT :: LHsExpr TcId  -> TcM (LHsType RdrName)
runMetaD :: LHsExpr TcId -> TcM [LHsDecl RdrName]

lookupThName_maybe :: TH.Name -> TcM (Maybe Name)
runQuasi :: TH.Q a -> TcM a
runRemoteModFinalizers :: ThModFinalizers -> TcM ()
finishTH :: TcM ()