{-# LANGUAGE CPP #-}

module GHC.Util.CodeGen.Platform.PPC_Darwin where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_powerpc 1
#define MACHREGS_darwin 1
#include "../../../../../includes/CodeGen.Platform.hs"
