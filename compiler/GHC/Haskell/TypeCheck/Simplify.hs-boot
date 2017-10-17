module GHC.Haskell.TypeCheck.Simplify where

import GHC.Prelude
import TcRnTypes  ( TcM )
import TcType ( TcSigmaType )

-- This boot file exists solely to make tcSubsume avaialble in TcErrors

tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
