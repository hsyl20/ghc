module GHC.Haskell.TypeCheck.Simplify where

import GHC.Prelude
import GHC.Haskell.TypeCheck.Util          ( TcM )
import GHC.Haskell.TypeCheck.Util.CoreType ( TcSigmaType )

-- This boot file exists solely to make tcSubsume avaialble in TcErrors

tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
