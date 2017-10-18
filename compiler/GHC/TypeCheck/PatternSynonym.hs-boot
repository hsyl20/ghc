module GHC.TypeCheck.PatternSynonym where

import GHC.Syntax            ( PatSynBind, LHsBinds )
import GHC.TypeCheck.Util ( TcM, TcPatSynInfo )
import GHC.TypeCheck.Monad       ( TcGblEnv)
import GHC.Util.Outputable             ( Outputable )
import GHC.Syntax.Extension  ( GhcRn, GhcTc )

tcInferPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcM (LHsBinds GhcTc, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcPatSynInfo
                  -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn -> TcM (LHsBinds GhcTc)

nonBidirectionalErr :: Outputable name => name -> TcM a
