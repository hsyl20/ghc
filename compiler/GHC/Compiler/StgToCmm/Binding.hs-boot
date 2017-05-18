module GHC.Compiler.StgToCmm.Binding where

import GHC.Compiler.StgToCmm.Monad ( FCode )
import GHC.IR.Stg.Syntax           ( StgBinding )

cgBind :: StgBinding -> FCode ()
