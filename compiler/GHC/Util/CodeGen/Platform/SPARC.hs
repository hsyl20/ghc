{-# LANGUAGE CPP #-}

module GHC.Util.CodeGen.Platform.SPARC where

import GHC.Prelude

#define MACHREGS_NO_REGS 0
#define MACHREGS_sparc 1
#include "../../../../../includes/CodeGen.Platform.hs"
