{-# LANGUAGE CPP #-}

module GHC.Util.CodeGen.Platform.NoRegs where

import GHC.Prelude

#define MACHREGS_NO_REGS 1
#include "../../../../../includes/CodeGen.Platform.hs"
