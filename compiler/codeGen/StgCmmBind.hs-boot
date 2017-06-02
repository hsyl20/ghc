module StgCmmBind where

import StgCmmMonad( FCode )
import GHC.IR.Stg.Syntax( StgBinding )

cgBind :: StgBinding -> FCode ()
