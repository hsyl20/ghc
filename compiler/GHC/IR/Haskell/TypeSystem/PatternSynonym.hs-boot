module GHC.IR.Haskell.TypeSystem.PatternSynonym where

import GHC.Data.Name      ( Name )
import GHC.Data.Id        ( Id )
import GHC.IR.Haskell.Syntax ( PatSynBind, LHsBinds )
import GHC.IR.Haskell.TypeSystem.Types ( TcM, TcPatSynInfo )
import GHC.IR.Haskell.TypeSystem ( TcGblEnv)
import GHC.Utils.Outputable ( Outputable )

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (LHsBinds Id, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (LHsBinds Id, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind Name Name -> TcM (LHsBinds Id)

nonBidirectionalErr :: Outputable name => name -> TcM a
