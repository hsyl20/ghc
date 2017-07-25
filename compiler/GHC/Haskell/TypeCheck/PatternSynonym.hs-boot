module GHC.Haskell.TypeCheck.PatternSynonym where

import GHC.Haskell.Syntax            ( PatSynBind, LHsBinds )
import GHC.Haskell.TypeCheck.Types ( TcM, TcPatSynInfo )
import GHC.Haskell.TypeCheck       ( TcGblEnv)
import GHC.Utils.Outputable             ( Outputable )
import GHC.Haskell.Syntax.Extension  ( GhcRn, GhcTc )

tcInferPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcM (LHsBinds GhcTc, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind GhcRn GhcRn
                  -> TcPatSynInfo
                  -> TcM (LHsBinds GhcTc, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind GhcRn GhcRn -> TcM (LHsBinds GhcTc)

nonBidirectionalErr :: Outputable name => name -> TcM a
