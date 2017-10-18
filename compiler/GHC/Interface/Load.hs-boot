module GHC.Interface.Load where
import GHC.CoreTypes.Module          (Module)
import GHC.TypeCheck.Monad (IfM)
import GHC.CoreTypes.Base           (ModIface)
import GHC.Util.Outputable       (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
