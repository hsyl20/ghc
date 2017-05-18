module GHC.IR.Haskell.TypeChecker.PatternSynonym where

import GHC.IR.Haskell.Syntax           ( PatSynBind, LHsBinds )
import GHC.IR.Haskell.TypeChecker.Types ( TcM, TcPatSynInfo )
import GHC.IR.Haskell.TypeChecker       ( TcGblEnv)
import GHC.Utils.Outputable            ( Outputable )
import GHC.IR.Haskell.Syntax.Extension ( GhcRn, GhcTc )

tcInferPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcM (LHsBinds GhcTc, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcPatSynInfo
                  -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn -> TcM (LHsBinds GhcTc)

nonBidirectionalErr :: Outputable name => name -> TcM a
