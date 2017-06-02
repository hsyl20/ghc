-----------------------------------------------------------------------------
--
-- Code generation for coverage
--
-- (c) Galois Connections, Inc. 2006
--
-----------------------------------------------------------------------------

module GHC.Compilers.StgToCmm.Coverage ( initHpc, mkTickBox ) where

import GHC.Compilers.StgToCmm.Monad

import GHC.IR.Cmm.Graph
import GHC.IR.Cmm.Expr
import GHC.Data.CLabel
import GHC.Data.Module
import GHC.IR.Cmm.Utils
import GHC.Compilers.StgToCmm.Utils
import GHC.Types
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

