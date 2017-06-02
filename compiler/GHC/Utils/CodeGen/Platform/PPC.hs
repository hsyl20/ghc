{-# LANGUAGE CPP #-}

module GHC.Utils.CodeGen.Platform.PPC where

#define MACHREGS_NO_REGS 0
#define MACHREGS_powerpc 1
#include "../../../../includes/CodeGen.Platform.hs"

