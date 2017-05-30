module TcPatSyn where

import GHC.Data.Name      ( Name )
import Id        ( Id )
import HsSyn     ( PatSynBind, LHsBinds )
import TcRnTypes ( TcM, TcPatSynInfo )
import TcRnMonad ( TcGblEnv)
import GHC.Utils.Outputable ( Outputable )

tcInferPatSynDecl :: PatSynBind Name Name
                  -> TcM (LHsBinds Id, TcGblEnv)

tcCheckPatSynDecl :: PatSynBind Name Name
                  -> TcPatSynInfo
                  -> TcM (LHsBinds Id, TcGblEnv)

tcPatSynBuilderBind :: PatSynBind Name Name -> TcM (LHsBinds Id)

nonBidirectionalErr :: Outputable name => name -> TcM a
