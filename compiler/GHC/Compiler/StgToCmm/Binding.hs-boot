module GHC.Compiler.StgToCmm.Binding where

import GHC.Compiler.StgToCmm.Monad ( FCode )
import GHC.Stg.Syntax           ( StgBinding )

cgBind :: StgBinding -> FCode ()
