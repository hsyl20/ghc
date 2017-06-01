module StgCmmBind where

import StgCmmMonad( FCode )
import GHC.STG.Syntax( StgBinding )

cgBind :: StgBinding -> FCode ()
