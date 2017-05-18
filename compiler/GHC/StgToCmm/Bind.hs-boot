module GHC.StgToCmm.Bind where

import GHC.StgToCmm.Monad ( FCode )
import GHC.Stg.Syntax           ( StgBinding )

cgBind :: StgBinding -> FCode ()
