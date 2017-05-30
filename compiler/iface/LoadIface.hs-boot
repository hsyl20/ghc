module LoadIface where
import Module (Module)
import TcRnMonad (IfM)
import GHC.Types (ModIface)
import GHC.Utils.Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
