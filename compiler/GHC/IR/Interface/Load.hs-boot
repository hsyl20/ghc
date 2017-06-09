module GHC.IR.Interface.Load where
import GHC.Data.Module (Module)
import GHC.IR.Haskell.TypeSystem (IfM)
import GHC.Types (ModIface)
import GHC.Utils.Outputable (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
