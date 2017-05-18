module GHC.Interface.Load where
import GHC.Types.Module          (Module)
import GHC.Haskell.TypeCheck (IfM)
import GHC.Types.Base           (ModIface)
import GHC.Utils.Outputable       (SDoc)

loadSysInterface :: SDoc -> Module -> IfM lcl ModIface
