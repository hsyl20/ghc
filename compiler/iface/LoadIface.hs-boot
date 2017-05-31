module LoadIface where
import GHC.Data.Module (Module)
import TcRnMonad (IfM)
import GHC.Types (ModIface)
import GHC.Utils.Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
