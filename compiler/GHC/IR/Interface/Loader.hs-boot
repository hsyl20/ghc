module GHC.IR.Interface.Loader where
import GHC.Entity.Module          (Module)
import GHC.IR.Haskell.TypeChecker (IfM)
import GHC.Entity.Types           (ModIface)
import GHC.Utils.Outputable       (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
