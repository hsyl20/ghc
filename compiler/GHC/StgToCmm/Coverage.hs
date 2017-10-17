-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.Coverage ( initHpc, mkTickBox ) where

import GhcPrelude

import GHC.StgToCmm.Monad

import GHC.Cmm.Syntax.Graph
import GHC.Cmm.Syntax.Expression
import GHC.CoreTypes.CLabel
import GHC.CoreTypes.Module
import GHC.Cmm.Utils
import GHC.StgToCmm.Utils
import GHC.CoreTypes.Base
import GHC.Config.Flags

import Control.Monad

mkTickBox :: DynFlags -> Module -> Int -> CmmAGraph
mkTickBox dflags mod n
  = mkStore tick_box (CmmMachOp (MO_Add W64)
                                [ CmmLoad tick_box b64
                                , CmmLit (CmmInt 1 W64)
                                ])
  where
    tick_box = cmmIndex dflags W64
                        (CmmLit $ CmmLabel $ mkHpcTicksLabel $ mod)
                        n

initHpc :: Module -> HpcInfo -> FCode ()
-- Emit top-level tables for HPC and return code to initialise
initHpc _ (NoHpcInfo {})
  = return ()
initHpc this_mod (HpcInfo tickCount _hashNo)
  = do dflags <- getDynFlags
       when (gopt Opt_Hpc dflags) $
           do emitDataLits (mkHpcTicksLabel this_mod)
                           [ (CmmInt 0 W64)
                           | _ <- take tickCount [0 :: Int ..]
                           ]

