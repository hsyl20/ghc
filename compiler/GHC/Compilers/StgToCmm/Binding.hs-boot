module GHC.Compilers.StgToCmm.Binding where

import GHC.Compilers.StgToCmm.Monad( FCode )
import GHC.IR.Stg.Syntax( StgBinding )

cgBind :: StgBinding -> FCode ()
