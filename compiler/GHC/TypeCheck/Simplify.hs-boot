module GHC.TypeCheck.Simplify where

import GHC.Prelude
import GHC.TypeCheck.Util          ( TcM )
import GHC.TypeCheck.Util.CoreType ( TcSigmaType )

-- This boot file exists solely to make tcSubsume avaialble in TcErrors

tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
