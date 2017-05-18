module LoadIface where
import Module (Module)
import TcRnMonad (IfM)
import GHC.Types (ModIface)
import Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
